#define DEFAULT_CONNECTION_TIMEOUT 30

const char *curl_request_url(const char *url,
                             const char *filename,
                             int cache_time);

const char *curl_wait_next_url(int *errorCode, long* httpErrorCode);

const char *curl_last_url();

void curl_enable_debug();

int curl_pipelining_enabled();

int set_time_out();
