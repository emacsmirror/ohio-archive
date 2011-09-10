/* EML : 7/11/94
 * 
 * This file was originally pilfered from the sources to ytalk, and
 * has been unmodified with the following exceptions:
 *
 * 1) char items (type, filler, answer etc) were converted to u_char
 *               for compatibility with talk.h.
 * 2) short items where converted to arrays of two characters.
 * 3) items which were u_long are ifdefed into u_int when longs are
 *               not 4 bytes long (DEC alpha type stuff.)
 * 4) This comment has been added.
 *
 * In addition, I believe the comments refering to these structures as
 * pre-BSD4.2 should be sunOS structures since these protocols do not
 * work with so-called otalk daemons.
 *
 * Lastly, I beleive these structures are public domain since I never
 * saw copyright notices on this, or even reference to this anywhere.
 * If this is copywritten by someone, go hunt down the author of ytalk
 * who DBXed network daemons and packet trapped message to find this
 * information.
 *
 * For the aquisition of this file, I give thanks to:
 *  Britt Yenne, yenne@ccwf.cc.utexas.edu
 */

/* These are different than in the new talk protocol. */

#define ONAME_SIZE 9
#define OTTY_SIZE 16

/* Control Message structure for earlier than BSD4.2
 */
typedef struct {
	u_char	type;
	char	l_name[ONAME_SIZE];
	char	r_name[ONAME_SIZE];
	u_char	filler;
	u_long	id_num;
#if SIZEOF_LONG == 4
	u_long	pid;
#else
	u_int   pid;
#endif /* sizeof_long == 4 */
	char	r_tty[OTTY_SIZE];
	struct	sockaddr addr;
	struct	sockaddr ctl_addr;
} CTL_MSG_OLD;

/* Control Response structure for earlier than BSD4.2
 */
typedef struct {
	u_char	type;
	u_char	answer;
	u_char  filler[2];	/* must by 2 bytes */
#if SIZEOF_LONG == 4
	u_long	id_num;
#else
	u_int   id_num;
#endif /* sizeof_long == 4 */
	struct	sockaddr addr;
} CTL_RESPONSE_OLD;
