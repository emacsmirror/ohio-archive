/*
 * Response codes for NNTP server
 *
 * @(#)response_codes.h 1.6     (Berkeley) 2/6/86
 *
 * First digit:
 *
 *      1xx     Informative message
 *      2xx     Command ok
 *      3xx     Command ok so far, continue
 *      4xx     Command was correct, but couldn't be performed
 *              for some specified reason.
 *      5xx     Command unimplemented, incorrect, or a
 *              program error has occured.
 *
 * Second digit:
 *
 *      x0x     Connection, setup, miscellaneous
 *      x1x     Newsgroup selection
 *      x2x     Article selection
 *      x3x     Distribution
 *      x4x     Posting
 */

#define CHAR_INF        '1'
#define CHAR_OK         '2'
#define CHAR_CONT       '3'
#define CHAR_ERR        '4'
#define CHAR_FATAL      '5'

#define INF_HELP        100     /* Help text on way */
#define INF_DEBUG       199     /* Debug output */

#define OK_CANPOST      200     /* Hello; you can post */
#define OK_NOPOST       201     /* Hello; you can't post */
#define OK_SLAVE        202     /* Slave status noted */
#define OK_GOODBYE      205     /* Closing connection */
#define OK_GROUP        211     /* Group selected */
#define OK_GROUPS       215     /* Newsgroups follow */
#define OK_ARTICLE      220     /* Article (head & body) follows */
#define OK_HEAD         221     /* Head follows */
#define OK_BODY         222     /* Body follows */
#define OK_NOTEXT       223     /* No text sent -- stat, next, last */
#define OK_NEWNEWS      230     /* New articles by message-id follow */
#define OK_NEWGROUPS    231     /* New newsgroups follow */
#define OK_XFERED       235     /* Article transferred successfully */
#define OK_POSTED       240     /* Article posted successfully */

#define CONT_XFER       335     /* Continue to send article */
#define CONT_POST       340     /* Continue to post article */

#define ERR_GOODBYE     400     /* Have to hang up for some reason */
#define ERR_NOGROUP     411     /* No such newsgroup */
#define ERR_NCING       412     /* Not currently in newsgroup */
#define ERR_NOCRNT      420     /* No current article selected */
#define ERR_NONEXT      421     /* No next article in this group */
#define ERR_NOPREV      422     /* No previous article in this group */
#define ERR_NOARTIG     423     /* No such article in this group */
#define ERR_NOART       430     /* No such article at all */
#define ERR_GOTIT       435     /* Already got that article, don't send */
#define ERR_XFERFAIL    436     /* Transfer failed */
#define ERR_XFERRJCT    437     /* Article rejected, don't resend */
#define ERR_NOPOST      440     /* Posting not allowed */
#define ERR_POSTFAIL    441     /* Posting failed */

#define ERR_COMMAND     500     /* Command not recognized */
#define ERR_CMDSYN      501     /* Command syntax error */
#define ERR_ACCESS      502     /* Access to server denied */
#define ERR_FAULT       503     /* Program fault, command not performed */

/* nntp program interface error codes */
#define NN_ERR_OS_RANGE 2
#define NN_ERR_NOERR    0
#define NN_ERR_IO       1
#define NN_ERR_SKT      2
#define NN_ERR_CONN     3
#define NN_ERR_NOSVC    4
#define NN_ERR_NOHOST   5
#define NN_ERR_RESET    6
#define NN_ERR_PROTO    7

int nntp_out(), nntp_open_connection(), nntp_list_end_p(), 
    nntp_in(), nntp_command();
void nntp_close();
char *nntp_errstring();

#define TRUE 1
#define FALSE (!TRUE)
#define OK 0
#define ERROR (-1)

#define nntp_current_reply(nnp) ((nnp)->nn_reply)
#define nntp_current_reply_code(nnp) ((nnp)->nn_reply_code)
#define nntp_end_list(nnp) (nntp_out((nnp), "."))
#define nntp_errno(nnp) ((nnp)->nn_error)
#define nntp_os_errorp(nnp) (nntp_errno((nnp)) <= NN_ERR_OS_RANGE)

typedef struct {
    char nn_inbuf[512];
    char *nn_old_data_begin;
    char *nn_new_data_begin;
    char nn_reply[512];
    char nn_outbuf[512];
    int nn_reply_code;
    int nn_skt;
    int nn_error;
    int nn_nbytes;
} Nntp_stream;