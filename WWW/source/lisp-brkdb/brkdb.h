
int
brkdb_open(char *filename, int type, int flags, int mode, void **db);
int
brkdb_put(void *db, int keylen, char *keystr,
	  int vallen, char *valstr);
int
brkdb_get(void *db, int keylen, char *keystr,
	  int *datalen, char **data);
int
brkdb_close(void *db);
