#include "hscurl.h"

#include <curl/curl.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if LIBCURL_VERSION_NUM >= 0x071301
/* enable pipelining for libcurl >= 7.19.1 */
#define ENABLE_PIPELINING
#endif

enum RESULT_CODES
  {
    RESULT_OK = 0,
    RESULT_MALLOC_FAIL,
    RESULT_SELECT_FAIL,
    RESULT_MULTI_INIT_FAIL,
    RESULT_EASY_INIT_FAIL,
    RESULT_SLIST_APPEND_FAIL,
    RESULT_MULTI_INFO_READ_FAIL,
    RESULT_UNKNOWN_MESSAGE,
    RESULT_FILE_OPEN_FAIL
  };

static const char *error_strings[] =
  {
    "",
    "malloc() failed",
    "select() failed",
    "curl_multi_init() failed",
    "curl_easy_init() failed",
    "curl_slist_append() failed",
    "curl_multi_info_read() failed",
    "curl_multi_info_read() returned unknown message",
    "fopen() failed"
  };

struct UrlData
{
  char *url;
  FILE *file;
  struct curl_slist *headers;
};

static int debug = 0;
#ifndef _WIN32
static const char user_agent[] =
  "darcs/" PACKAGE_VERSION " libcurl/" LIBCURL_VERSION;
#else
static const char user_agent[] =
  "darcs/unknown libcurl/" LIBCURL_VERSION;
#endif

static const char *proxypass;
static int init_done = 0;
static CURLM *multi = NULL;
static int msgs_in_queue = 0;
static char *last_url = NULL;

static const char *perform()
{
  int error;
  int running_handles, running_handles_last;
  fd_set fd_read, fd_write, fd_except;
  int max_fd;
  long timeout;
  struct timeval tval;

  error = curl_multi_perform(multi, &running_handles);
  if (error != CURLM_OK && error != CURLM_CALL_MULTI_PERFORM)
    return curl_multi_strerror(error);

  running_handles_last = running_handles;
  while (running_handles_last > 0)
    {
      while (error == CURLM_CALL_MULTI_PERFORM)
        error = curl_multi_perform(multi, &running_handles);

      if (error != CURLM_OK)
        return curl_multi_strerror(error);

      if (running_handles < running_handles_last)
        break;

      FD_ZERO(&fd_read);
      FD_ZERO(&fd_write);
      FD_ZERO(&fd_except);

      error = curl_multi_fdset(multi, &fd_read, &fd_write, &fd_except, &max_fd);
      if (error != CURLM_OK && error != CURLM_CALL_MULTI_PERFORM)
        return curl_multi_strerror(error);

#ifdef CURL_MULTI_TIMEOUT
      error = curl_multi_timeout(multi, &timeout);
      if (error != CURLM_OK && error != CURLM_CALL_MULTI_PERFORM)
        return curl_multi_strerror(error);

      if (timeout == -1)
#endif
        timeout = 100;

      tval.tv_sec = timeout / 1000;
      tval.tv_usec = timeout % 1000 * 1000;

      while (select(max_fd + 1, &fd_read, &fd_write, &fd_except, &tval) < 0)
        if (errno != EINTR)
          {
            if (debug)
              perror(error_strings[RESULT_SELECT_FAIL]);
            return error_strings[RESULT_SELECT_FAIL];
          }

      error = CURLM_CALL_MULTI_PERFORM;
    }

  return NULL;
}

const char *curl_request_url(const char *url,
                             const char *filename,
                             int cache_time)
{
  int error;

  if (init_done == 0)
    {
      error = curl_global_init(CURL_GLOBAL_ALL);
      if (error != CURLE_OK)
        return curl_easy_strerror(error);
      proxypass = getenv("DARCS_PROXYUSERPWD");
      init_done = 1;
    }

  if (multi == NULL)
    {
      multi = curl_multi_init();
      if (multi == NULL)
        return error_strings[RESULT_MULTI_INIT_FAIL];
#ifdef ENABLE_PIPELINING
      error = curl_multi_setopt(multi, CURLMOPT_PIPELINING, 1);
      if (error != CURLM_OK && error != CURLM_CALL_MULTI_PERFORM)
        return curl_multi_strerror(error);
#endif
    }

  CURL *easy = curl_easy_init();
  if (easy == NULL)
    return error_strings[RESULT_EASY_INIT_FAIL];

  if (debug)
    {
      error = curl_easy_setopt(easy, CURLOPT_VERBOSE, 1);
      if (error != CURLE_OK)
        return curl_easy_strerror(error);
    }

  struct UrlData *url_data = malloc(sizeof(struct UrlData));
  if (url_data == NULL)
    return error_strings[RESULT_MALLOC_FAIL];

  url_data->url = strdup(url);
  if (url_data->url == NULL)
    return error_strings[RESULT_MALLOC_FAIL];

  url_data->file = fopen(filename,"wb");
  if (url_data->file == NULL)
    {
      if (debug)
        perror(error_strings[RESULT_FILE_OPEN_FAIL]);
      return error_strings[RESULT_FILE_OPEN_FAIL];
    }

  error = curl_easy_setopt(easy, CURLOPT_PRIVATE, url_data);
  if (error != CURLE_OK)
    return curl_easy_strerror(error);

  error = curl_easy_setopt(easy, CURLOPT_URL, url_data->url);
  if (error != CURLE_OK)
    return curl_easy_strerror(error);

#ifdef CURLOPT_WRITEDATA
  error = curl_easy_setopt(easy, CURLOPT_WRITEDATA, url_data->file);
#else
  error = curl_easy_setopt(easy, CURLOPT_FILE, url_data->file);
#endif
  if (error != CURLE_OK)
    return curl_easy_strerror(error);

  error = curl_easy_setopt(easy, CURLOPT_USERAGENT, user_agent);
  if (error != CURLE_OK)
    return curl_easy_strerror(error);

  error = curl_easy_setopt(easy, CURLOPT_FOLLOWLOCATION, 1);
  if (error != CURLE_OK)
    return curl_easy_strerror(error);

  error = curl_easy_setopt(easy, CURLOPT_FAILONERROR, 1);
  if (error != CURLE_OK)
    return curl_easy_strerror(error);

  error = curl_easy_setopt(easy, CURLOPT_HTTPAUTH, CURLAUTH_ANY);
  if (error != CURLE_OK)
    return curl_easy_strerror(error);

  /* libcurl currently always sends Pragma: no-cache, but never
     Cache-Control, which is contradictory.  We override both, just to
     be sure. */
  url_data->headers = curl_slist_append(NULL, "Accept: */*");
  if(cache_time == 0)
    {
      url_data->headers =
        curl_slist_append(url_data->headers, "Pragma: no-cache");
      url_data->headers =
        curl_slist_append(url_data->headers, "Cache-Control: no-cache");
    }
  else if(cache_time > 0)
    {
      /* This won't work well with HTTP/1.0 proxies. */
      char buf[40];
      snprintf(buf, sizeof(buf), "Cache-Control: max-age=%d", cache_time);
      buf[sizeof(buf) - 1] = '\n';
      url_data->headers = curl_slist_append(url_data->headers, "Pragma:");
      url_data->headers = curl_slist_append(url_data->headers, buf);
    }
  else
    {
      url_data->headers = curl_slist_append(url_data->headers, "Pragma:");
      url_data->headers = curl_slist_append(url_data->headers, "Cache-Control:");
    }
  if (url_data->headers == NULL)
    return error_strings[RESULT_SLIST_APPEND_FAIL];

  error = curl_easy_setopt(easy, CURLOPT_HTTPHEADER, url_data->headers);
  if (error != CURLE_OK)
    return curl_easy_strerror(error);

  if (proxypass && *proxypass)
    {
      error = curl_easy_setopt(easy, CURLOPT_PROXYUSERPWD, proxypass);
      if (error != CURLE_OK)
        return curl_easy_strerror(error);
    }

  error = curl_multi_add_handle(multi, easy);
  if (error != CURLM_OK && error != CURLM_CALL_MULTI_PERFORM)
    return curl_multi_strerror(error);

  return error_strings[RESULT_OK];
}

const char *curl_wait_next_url(int* errorCode, long* httpErrorCode)
{
  *errorCode = -1;
  *httpErrorCode = -1;

  if (last_url != NULL)
    {
      free(last_url);
      last_url = NULL;
    }

  if (msgs_in_queue == 0)
    {
      const char *error = perform();
      if (error != NULL)
        return error;
    }

  CURLMsg *msg = curl_multi_info_read(multi, &msgs_in_queue);
  if (msg == NULL)
    return error_strings[RESULT_MULTI_INFO_READ_FAIL];

  if (msg->msg == CURLMSG_DONE)
    {
      CURL *easy = msg->easy_handle;
      CURLcode result = msg->data.result;
      struct UrlData *url_data;

      int error = set_time_out(easy, errorCode);
      if (error != CURLE_OK ){
        *errorCode = error;
        return curl_easy_strerror(error);
      }

      error = curl_easy_getinfo(easy, CURLINFO_PRIVATE, (char **)&url_data);
      if (error != CURLE_OK){
        *errorCode = error;
        return curl_easy_strerror(error);
      }

      last_url = url_data->url;
      fclose(url_data->file);
      curl_slist_free_all(url_data->headers);
      free(url_data);

      error = curl_multi_remove_handle(multi, easy);
      if (error != CURLM_OK && error != CURLM_CALL_MULTI_PERFORM)
        return curl_multi_strerror(error);
      curl_easy_cleanup(easy);

      if (result != CURLE_OK){
        *errorCode = result;

        if (result == CURLE_HTTP_RETURNED_ERROR)
		  curl_easy_getinfo(easy, CURLINFO_RESPONSE_CODE, httpErrorCode);

        return curl_easy_strerror(result);
      }
    }
  else
    return error_strings[RESULT_UNKNOWN_MESSAGE];

  return error_strings[RESULT_OK];
}

const char *curl_last_url()
{
  return last_url != NULL ? last_url : "";
}

void curl_enable_debug()
{
  debug = 1;
}

int curl_pipelining_enabled()
{
#ifdef ENABLE_PIPELINING
  return 1;
#else
  return 0;
#endif
}

int set_time_out(CURL *handle, int* errorCode)
{
  int error;
  long time_out = DEFAULT_CONNECTION_TIMEOUT;
  const char *stime_out;

  stime_out = getenv("DARCS_CONNECTION_TIMEOUT");
  if (stime_out != NULL){
    long result = atol (stime_out);
    if ( result > 0 )
      time_out = result;
    else
      *errorCode = 90 ;
  }

  error = curl_easy_setopt(handle, CURLOPT_TIMEOUT, time_out);

  return error;
}
