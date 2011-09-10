(in-package :elispdb)

(defvar *araneida-site-name* "HOSTNAMEHERE"
  "Serve the Araneida docs and examples from this hostname.  Must
be an alias for (or the name of) the machine that Araneida is running on")
(defvar *araneida-source-directory* #p"/usr/local/src/araneida/"
  "The directory in which to find source for the running Araneida")

(defvar *static-site-name* "HOSTNAMEHERE"
  "Serve some unrelated static pages from this hostname.  Must
be an alias for (or the name of) the machine that Araneida is running on")
(defvar *static-pages-directory* #p"/usr/share/doc/"
  "The root directory that  *static-site-name* serves content from")

(defvar *apache-config-segment* "/tmp/apache.cf.inc"
  "Pathname that we should write with our bit of apache configuration")

(defvar *mime-types-file* "/etc/mime.types"
  "Pathname to system mime.types file (or NIL if none exists)")
