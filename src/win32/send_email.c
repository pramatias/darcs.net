

#include <windows.h>
#include <mapi.h>
#include <stdio.h>

#include "send_email.h"

typedef struct sMapiFuns {
    LPMAPILOGON logon;
    LPMAPISENDMAIL sendmail;
    LPMAPIRESOLVENAME resolve;
    LPMAPIFREEBUFFER free_buf;
    LPMAPILOGOFF logoff;
    HMODULE dll;
} MapiFuns;


int load_dll(const char* name, MapiFuns* funs);
void free_dll(MapiFuns* funs);

void get_recipient(MapiFuns* funs, const char *name, ULONG recipClass, MapiRecipDesc *desc, lpMapiRecipDesc *desc_lookup);

int send_email(const char *sendname, 
               const char *recvname, 
               const char *ccname, 
               const char *subj,
               const char *body,
               const char *path)
{
    FLAGS flags;
    MapiMessage msg;
    ULONG send_res;
    MapiRecipDesc orig;
    MapiRecipDesc recips[2];
    MapiRecipDesc *orig_lookup, *recip_lookup, *cc_lookup;
    int num_recip = 1, return_code = -1;
    MapiFileDesc attachment;
    MapiFileTagExt file_type;
    const char *filename;
    char *attachment_path = 0;
    MapiFuns funs;
    
    if(load_dll("mapistub.dll", &funs) || load_dll("mapi32.dll", &funs)) {
        return_code=0;
    } else {
        fprintf(stderr, "Unable to load mapistub.dll or mapi32.dll: Bailing out. \n");
        return_code=-1;
    }

    if(return_code==0) {
        LHANDLE session;
        /* logon seems to be necessary for outlook express, sometimes,
        and doesn't seem to hurt, otherwise.
        */
        funs.logon(0, 0, 0, MAPI_LOGON_UI, 0, &session);

        
        orig_lookup = recip_lookup = cc_lookup = NULL;
        
        get_recipient(&funs, sendname, MAPI_ORIG, &orig, &orig_lookup);
        get_recipient(&funs, recvname, MAPI_TO, &recips[0], &recip_lookup);

        if (ccname && strlen(ccname) > 0) {
            get_recipient(&funs, ccname, MAPI_CC, &recips[1], &cc_lookup);
            num_recip++;
        }

        memset(&msg, 0, sizeof(msg));
        msg.lpOriginator = &orig;
        msg.lpRecips = recips;
        msg.lpszMessageType = "text/plain";
        msg.lpszNoteText = (LPSTR) body;
        msg.lpszSubject = (LPSTR)subj;
        msg.nRecipCount = num_recip;
        msg.flFlags = 0;

        if (path) {
            attachment_path = strdup(path);
            /* convert / to \  (thunderbird doesn't like /) */
            char *p = attachment_path;
            while ((p = strchr(p, '/')))
                *p = '\\';

            /* find filename */
            filename = strrchr(attachment_path, '\\');
            if (filename == 0)
                filename = attachment_path;
            else
                filename++;

            memset(&attachment, 0, sizeof(attachment));
            attachment.nPosition = -1;
            attachment.lpszPathName = (LPTSTR)attachment_path;
            attachment.lpszFileName = (LPTSTR)filename;

            attachment.lpFileType = &file_type;
            
            memset(&file_type, 0, sizeof(file_type));
            file_type.lpTag = "text/plain";
            file_type.cbTag = sizeof(file_type.lpTag);

            msg.nFileCount = 1;
            msg.lpFiles = &attachment;
        }

        flags = 0;
        send_res = funs.sendmail(0, 0, &msg, flags, 0);

        if (send_res == SUCCESS_SUCCESS)
            return_code = 0;
        else {
            return_code=-1;
            if(send_res==MAPI_E_USER_ABORT)                 fprintf(stderr, "MAPI error: User aborted.\n");
            else if(send_res== MAPI_E_FAILURE)              fprintf(stderr, "MAPI error: Generic error.\n");
            else if(send_res== MAPI_E_LOGIN_FAILURE)        fprintf(stderr, "MAPI error: Login failure.\n");
            else if(send_res== MAPI_E_DISK_FULL)            fprintf(stderr, "MAPI error: Disk full.\n");
            else if(send_res== MAPI_E_INSUFFICIENT_MEMORY)  fprintf(stderr, "MAPI error: Insufficient memory.\n");
            else if(send_res== MAPI_E_ACCESS_DENIED)        fprintf(stderr, "MAPI error: Access denied.\n");
            else if(send_res== MAPI_E_TOO_MANY_SESSIONS)    fprintf(stderr, "MAPI error: Too many sessions\n");
            else if(send_res== MAPI_E_TOO_MANY_FILES)       fprintf(stderr, "MAPI error: Too many files.\n");
            else if(send_res== MAPI_E_TOO_MANY_RECIPIENTS)  fprintf(stderr, "MAPI error: Too many recipients.\n");
            else if(send_res== MAPI_E_ATTACHMENT_NOT_FOUND) fprintf(stderr, "MAPI error: Attachment not found.\n");
            else if(send_res== MAPI_E_ATTACHMENT_OPEN_FAILURE)  fprintf(stderr, "MAPI error: Failed to open attachment.\n");
            else if(send_res== MAPI_E_ATTACHMENT_WRITE_FAILURE) fprintf(stderr, "MAPI error: Failed to write attachment.\n");
            else if(send_res== MAPI_E_UNKNOWN_RECIPIENT)    fprintf(stderr, "MAPI error: Unknown recipient\n");
            else if(send_res== MAPI_E_BAD_RECIPTYPE)        fprintf(stderr, "MAPI error: Bad type of recipent.\n");
            else if(send_res== MAPI_E_NO_MESSAGES)          fprintf(stderr, "MAPI error: No messages.\n");
            else if(send_res== MAPI_E_INVALID_MESSAGE)      fprintf(stderr, "MAPI error: Invalid message.\n");
            else if(send_res== MAPI_E_TEXT_TOO_LARGE)       fprintf(stderr, "MAPI error: Text too large.\n");
            else if(send_res== MAPI_E_INVALID_SESSION)      fprintf(stderr, "MAPI error: Invalid session.\n");
            else if(send_res== MAPI_E_TYPE_NOT_SUPPORTED)   fprintf(stderr, "MAPI error: Type not supported.\n");
            else if(send_res== MAPI_E_AMBIGUOUS_RECIP)      fprintf(stderr, "MAPI error: Ambigious recipient.\n");
            else if(send_res== MAPI_E_MESSAGE_IN_USE)       fprintf(stderr, "MAPI error: Messag in use.\n");
            else if(send_res== MAPI_E_NETWORK_FAILURE)      fprintf(stderr, "MAPI error: Network failure.\n");
            else if(send_res== MAPI_E_INVALID_EDITFIELDS)   fprintf(stderr, "MAPI error: Invalid editfields\n");
            else if(send_res== MAPI_E_INVALID_RECIPS)       fprintf(stderr, "MAPI error: Invalid recipient(s)\n");
            else if(send_res== MAPI_E_NOT_SUPPORTED)        fprintf(stderr, "MAPI error: Operation not supported.\n");
            else fprintf(stderr, "MAPISendMail returned %ld\n", send_res);
        }

        
        if (orig_lookup) funs.free_buf(orig_lookup);
        if (recip_lookup) funs.free_buf(recip_lookup);
        if (cc_lookup) funs.free_buf(cc_lookup);
        if (attachment_path) free(attachment_path);

        funs.logoff(session, 0, 0, 0);
    }
    free_dll(&funs);
    return return_code;
}

void get_recipient(MapiFuns* funs, const char *name, ULONG recipClass, MapiRecipDesc *desc, lpMapiRecipDesc *desc_lookup) {
    ULONG ret = funs->resolve(0, 0, (LPSTR) name, 0, 0, desc_lookup);
    if (ret == SUCCESS_SUCCESS) {
        memcpy(desc, *desc_lookup, sizeof(MapiRecipDesc));
    } else {
        /* Default to something sensible if MAPIResolveName is not supported 
         * by the mail client (thunderbird)
         */
        memset(desc, 0, sizeof(MapiRecipDesc));
        desc->lpszName = (LPSTR)name;
        desc->lpszAddress = (LPSTR)name;
        desc->lpEntryID = 0;
        desc->ulEIDSize = 0;
    }
    desc->ulRecipClass = recipClass;
}


int load_dll(const char* name, MapiFuns* funs) {
    funs->dll = 0;
    funs->dll = LoadLibrary(name);
    if(funs->dll!=NULL) {
        /* We try first loading by easy name, then by ordinal, and then by other names seen */
        funs->logon     = (LPMAPILOGON)         GetProcAddress(funs->dll, "MAPILogon");
        if(funs->logon==NULL)
            funs->logon     = (LPMAPILOGON)         GetProcAddress(funs->dll, (LPCSTR)209);

        funs->logoff    = (LPMAPILOGOFF)        GetProcAddress(funs->dll, "MAPILogOff");
        if(funs->logoff==NULL)
            funs->logoff    = (LPMAPILOGOFF)        GetProcAddress(funs->dll, (LPCSTR)210);

        funs->resolve   = (LPMAPIRESOLVENAME)   GetProcAddress(funs->dll, "MAPIResolveName");
        if(funs->resolve==NULL)
            funs->resolve   = (LPMAPIRESOLVENAME)   GetProcAddress(funs->dll, (LPCSTR)219);

        funs->free_buf  = (LPMAPIFREEBUFFER)    GetProcAddress(funs->dll, "MAPIFreeBuffer");
        if(funs->free_buf==NULL)
            funs->free_buf  = (LPMAPIFREEBUFFER)    GetProcAddress(funs->dll, (LPCSTR)16);
        if(funs->free_buf==NULL)
            funs->free_buf  = (LPMAPIFREEBUFFER)    GetProcAddress(funs->dll, "MAPIFreeBuffer@4");

        funs->sendmail  = (LPMAPISENDMAIL)      GetProcAddress(funs->dll, "MAPISendMail");
        if(funs->sendmail==NULL)
            funs->sendmail  = (LPMAPISENDMAIL)      GetProcAddress(funs->dll, (LPCSTR)211);
    }
    return 
            funs->dll!=NULL
        &&  funs->logon!=NULL
        &&  funs->logoff!=NULL
        &&  funs->resolve!=NULL
        &&  funs->free_buf!=NULL
        &&  funs->sendmail!=NULL;
}
void free_dll(MapiFuns* funs) {
    if(funs->dll!=NULL) FreeLibrary(funs->dll);
    funs->dll=NULL;
}

