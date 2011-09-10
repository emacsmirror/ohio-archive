
/* @(#)jnews.c	1.2 2/14/90 */

/* Written by Joshua Marantz, Viewlogic Systems Inc, February 1990 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/dir.h>

FILE *headerfile;

main(argc, argv)
    int argc;
    char *argv[];
{
    int article, last_header, last_article, first_article;
    char dir[256];
    
    if ((argc != 2) && (argc != 3)) {
        fprintf (stderr, "usage: %s news.group [newsgroup-directory]\n",
                 argv[0]);
        exit (1);
    }

    /* If the news group directory was not specified, figure it out */
    get_newsgroup_directory (argv[1],
                             (argc == 2) ? "/usr/spool/news" : argv[2],
                             dir);

    /* Find the first and last articles in the directory */
    max_articles (dir, &first_article, &last_article);

    /* Open the header file, and find the number of the last article in it */
    last_header = open_and_scan_headerfile (argv[1]);

    /* Add in the articles that are yet in the header file */
    if (first_article < last_header + 1)
        first_article = last_header + 1;
    for (article = first_article; article <= last_article; article++)
        add_to_headerfile (article, dir);

    fclose (headerfile);
}

get_newsgroup_directory(newsname, root, dir)
    char *newsname, *root, *dir;
{
    int i;
    sprintf (dir, "%s/%s", root, newsname);
    for (i = strlen (root); dir[i] != '\0'; i++)
        if (dir[i] == '.')
            dir[i] = '/';
}
            
max_articles(dirname, first, last)
    char *dirname;
    int *first, *last;
{
    int i, j, article;
    DIR *dir;
    struct direct *readdir (), *dp;

    if ((dir = opendir (dirname)) == NULL) {
        perror (dir);
        exit (1);
    }

    *first = 32767;
    *last = 0;

    for (dp = readdir (dir); dp != NULL; dp = readdir (dir)) {
        if ((sscanf (dp -> d_name, "%d", &article) == 1) &&
            (article > 0))
        {
            if (article > *last)
                *last = article;
            if (article < *first)
                *first = article;
        }
    }
    closedir (dir);
}

open_and_scan_headerfile(headername)
    char *headername;
{
    char buf[256];
    char *jnews_signature = "-*-jnews-*-\n";
    int article_number = 0;

    headerfile = fopen (headername, "r+");
    if (headerfile == NULL) {
        headerfile = fopen (headername, "w");
        if (headerfile == NULL) {
            perror (headername);
            exit (1);
        }
        fputs (jnews_signature, headerfile);
    }

    /* If the file exists, make sure its got the signature! */
    else if ((fgets (buf, 255, headerfile) == NULL) ||
             (strcmp (buf, jnews_signature) != 0))
    {
        fprintf (stderr, "Corrupt jnews header file: %s != %s\n",
                 buf, jnews_signature);
        fclose (headerfile);
        exit (1);
    }

    /* Jump to the last line of the file, which should be exactly 80
       bytes from the end of the file. */
    else if (fseek (headerfile, -80L, 2) != 0) {
        fclose (headerfile);
        perror ("Could not seek to end-80 in jnews header file");
        exit (1);
    }
    else if (fgets (buf, 255, headerfile) == NULL) {
        fclose (headerfile);
        perror ("Could not read the last 80 bytes of jnews header file");
        exit (1);
    }
    else if ((sscanf (&buf[1], "%d|", &article_number) != 1) ||
             (article_number == 0))
    {
        fclose (headerfile);
        fprintf (stderr,
                 "Could not scan the last article number from line:\n%s",
                 buf);
        exit (1);
    }
    fseek (headerfile, 0, 2);
    return (article_number);
}
            
add_to_headerfile(article, dir)
    int article;
    char *dir;
{
    char buf[256], subject[80], date[80], lines[80], from[80], *author;
    char fname[256];
    FILE *f;
    int matches = 0;
    int i, len;

    sprintf (fname, "%s/%d", dir, article);
    if ((f = fopen (fname, "r")) == NULL) {
        perror (fname);
        return;
    }

    from[0] = subject[0] = date[0] = lines[0] = '\0';

    while ((matches < 4) && (fgets (buf, 255, f) != NULL)) {

        /* Chop off the last \n */
        len = strlen (buf);
        if ((len > 0) && (buf[len - 1] == '\n'))
            buf[len - 1] = '\0';
        
        /* Assume we will get a match and decrement if we do not! */
        matches++;
        if (strncmp (buf, "From: ", 6) == 0)
            strcpy (from, &buf[6]);
        else if (strncmp (buf, "Date: ", 6) == 0)
            strcpy (date, &buf[6]);
        else if (strncmp (buf, "Subject: ", 9) == 0)
            strcpy (subject, &buf[9]);
        else if (strncmp (buf, "Lines: ", 7) == 0)
            strcpy (lines, &buf[7]);
        else
            matches--;
    }

    fclose (f);

    /* Transform the subject to a human name if supplied. */
    if (((len = strlen (from)) == 0) || (from[len - 1] != ')'))
        author = from;
    else {
        /* Copy the string right justified inside parens */
        for (i = len - 1; (i >= 0) && (from[i] != '('); i--);
        if (i >= 0) {
            from[len - 1] = '\0';
            author = &from[i + 1];
        }
    }

    date[9] = author[18] = subject[37] = lines[5] = '\0';

    /* If the date takes only one digit, chop off the space at the end */
    if (date[strlen (date) - 1] == ' ')
        date[strlen (date) - 1] = '\0';

    fprintf (headerfile, "*%5d|%37s|%9s|%18s|%5s\n",
             article, subject, date, author, lines);
}
