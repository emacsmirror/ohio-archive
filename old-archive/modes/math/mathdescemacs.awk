# build an array of action names
BEGIN	{ print "char *action_name[] = {" }
	{ print "   \"",$1 }
END	{ print "  }" }
