#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <db.h>

void *
brkdb_open(char *filename)
{
  DB *dbp;
  
  if (db_open(filename, DB_BTREE, DB_CREATE, 0644, NULL, NULL, &dbp) == 0)
    return (void *) dbp;

  return 0;
}

int
brkdb_put(void *db, char *keystr, char *valstr)
{
  DBT key, val;
  /* beautiful, isn't it? */
  DB *dbp = (DB *) db;
  
  memset(&key, 0, sizeof(key));
  memset(&val, 0, sizeof(val));

  key.data = keystr;
  key.size = strlen(keystr) + 1;

  val.data = valstr;
  val.size = strlen(valstr) + 1;
  
  return dbp->put(dbp, NULL, &key, &val, 0);
}

char *
brkdb_get(void *db, char *keystr)
{
  DBT key, val;
  /* beautiful, isn't it? */
  DB *dbp = (DB *) db;
  int ret;

  memset(&key, 0, sizeof(key));  
  memset(&val, 0, sizeof(val));
  
  key.data = keystr;
  key.size = strlen(keystr) + 1;

  ret = dbp->get(dbp, NULL, &key, &val, 0);
  if (ret == DB_NOTFOUND)
    return "";
  else
    return (char *) val.data;
}

int
brkdb_close(void *db)
{
  DB *dbp = (DB *) db;
  dbp->close(dbp, 0);
  return 0;
}

  
  
  

