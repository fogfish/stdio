
-define(CONFIG_CHUNK_READER, 65536).

-record(iostream, {
   module = undefined :: atom(),  %% stream module 
   fd     = undefined :: _        %% stream dependent file descriptor
}).
