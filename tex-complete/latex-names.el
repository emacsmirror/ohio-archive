;; latex-names.el

(defconst latex-names-version
  "$Id: latex-names.el,v 1.2 90/06/19 18:31:50 sk Exp $"
  "The RCS id of this version of latex-names.")

(provide 'latex-names)

(defconst latex-all-environments
'(
("BIG")
("Big")
("abstract")
("array")
("big")
("center")
("description")
("displaymath")
("document")
("enumerate")
("eqnarray")
("eqnarray")
("equation")
("figure")
("figure")
("float")
("flushleft")
("flushright")
("footnotesize")
("itemize")
("list")
("math")
("minipage")
("obeycr")
("picture")
("quotation")
("quote")
("sloppypar")
("small")
("tabbing")
("table")
("table*")
("tabular")
("tabular*")
("thebibliography")
("theindex")
("titlepage")
("trivlist")
("verbatim")
("verbatim*")
("verse")
   ; add others that you miss:
("letter")
)
"List of all LaTeX environments.")

(defconst latex-lplain-commands
'(
("!")
("\"")
("#")
("$")
("%")
("&")
("'")
("*")
(",")
(".")
(";")
("=")
(">")
("AA")
("AE")
("Arrowvert")
("Big")
("Bigg")
("Biggl")
("Biggm")
("Biggr")
("Bigl")
("Bigm")
("Bigr")
("Delta")
("Downarrow")
("Gamma")
("H")
("Im")
("L")
("Lambda")
("Leftarrow")
("Leftrightarrow")
("Longleftarrow")
("Longleftrightarrow")
("Longrightarrow")
("O")
("OE")
("Omega")
("P")
("Phi")
("Pi")
("Pr")
("Psi")
("Re")
("Rightarrow")
("S")
("Sigma")
("TeX")
("Theta")
("Uparrow")
("Updownarrow")
("Upsilon")
("Vert")
("Xi")
("^")
("_")
("`")
("aa")
("active")
("acute")
("ae")
("aleph")
("allowbreak")
("alpha")
("amalg")
("angle")
("approx")
("arccos")
("arcsin")
("arctan")
("arg")
("arrowvert")
("ast")
("asymp")
("b")
("backslash")
("bar")
("beta")
("bgroup")
("big")
("bigbreak")
("bigcap")
("bigcirc")
("bigcup")
("bigg")
("biggl")
("biggm")
("biggr")
("bigl")
("bigm")
("bigodot")
("bigoplus")
("bigotimes")
("bigr")
("bigsqcup")
("bigtriangledown")
("bigtriangleup")
("biguplus")
("bigvee")
("bigwedge")
("bmod")
("body")
("bordermatrix")
("bot")
("bowtie")
("brace")
("braceld")
("bracelu")
("bracerd")
("braceru")
("bracevert")
("brack")
("break")
("breve")
("buildrel")
("bullet")
("c")
("cap")
("cases")
("cdot")
("cdotp")
("cdots")
("check")
("chi")
("choose")
("circ")
("clubsuit")
("colon")
("cong")
("coprod")
("copyright")
("cos")
("cosh")
("cot")
("coth")
("cr")
("csc")
("cup")
("d")
("dag")
("dagger")
("dashv")
("ddag")
("ddagger")
("ddot")
("ddots")
("defaulthyphenchar")
("defaultskewchar")
("deg")
("delta")
("det")
("diamond")
("diamondsuit")
("dim")
("displaylines")
("div")
("dot")
("doteq")
("dotfill")
("dots")
("downarrow")
("downbracefill")
("eject")
("ell")
("empty")
("emptyset")
("endgraf")
("enskip")
("enspace")
("epsilon")
("equiv")
("eta")
("exists")
("exp")
("filbreak")
("flat")
("footnoterule")
("forall")
("frenchspacing")
("frown")
("gamma")
("gcd")
("geq")
("gg")
("goodbreak")
("grave")
("hang")
("hat")
("hbar")
("heartsuit")
("hfill")
("hglue")
("hidewidth")
("hom")
("hookleftarrow")
("hookrightarrow")
("hphantom")
("hrulefill")
("i")
("ialign")
("iff")
("imath")
("in")
("inf")
("infty")
("int")
("intop")
("iota")
("itemitem")
("j")
("jmath")
("jot")
("kappa")
("ker")
("l")
("lambda")
("langle")
("lbrace")
("lbrack")
("lceil")
("ldotp")
("ldots")
("leftarrow")
("leftarrowfill")
("leftharpoondown")
("leftharpoonup")
("leftline")
("leftrightarrow")
("leq")
("lfloor")
("lg")
("lgroup")
("lhook")
("lim")
("liminf")
("limsup")
("linepenalty")
("ll")
("lmoustache")
("ln")
("log")
("longleftarrow")
("longleftrightarrow")
("longmapsto")
("longrightarrow")
("loop")
("lq")
("mapsto")
("max")
("maxdepth")
("maxdimen")
("medbreak")
("medmuskip")
("mid")
("min")
("models")
("mp")
("mu")
("nabla")
("narrower")
("natural")
("nearrow")
("neg")
("negthinspace")
("neq")
("newtoks")
("ni")
("nonfrenchspacing")
("normalbaselines")
("not")
("notin")
("nu")
("null")
("nwarrow")
("o")
("oalign")
("obeylines")
("obeyspaces")
("odot")
("oe")
("oint")
("ointop")
("omega")
("ominus")
("oplus")
("oslash")
("otimes")
("overbrace")
("overfullrule")
("overleftarrow")
("overrightarrow")
("parallel")
("parfillskip")
("parindent")
("parskip")
("partial")
("pb")
("pc")
("pd")
("perp")
("phi")
("pi")
("pm")
("pmod")
("prec")
("preceq")
("pretolerance")
("prime")
("prod")
("propto")
("psi")
("qquad")
("quad")
("rangle")
("rbrace")
("rbrack")
("rceil")
("rfloor")
("rgroup")
("rho")
("rhook")
("rightarrow")
("rightarrowfill")
("rightharpoondown")
("rightharpoonup")
("rightleftharpoons")
("rmoustache")
("rq")
("scriptspace")
("searrow")
("sec")
("setminus")
("sharp")
("showboxbreadth")
("showboxdepth")
("sigma")
("sim")
("simeq")
("sin")
("sinh")
("slash")
("smallbreak")
("smallint")
("smash")
("smile")
("sp")
("spadesuit")
("sqcap")
("sqcup")
("sqrt")
("sqsubseteq")
("sqsupseteq")
("ss")
("star")
("strut")
("subset")
("subseteq")
("succ")
("succeq")
("sum")
("sup")
("supset")
("supseteq")
("surd")
("swarrow")
("t")
("tan")
("tanh")
("tau")
("theta")
("thickmuskip")
("thinmuskip")
("thinspace")
("tilde")
("times")
("top")
("topskip")
("triangle")
("triangleleft")
("triangleright")
("u")
("underbrace")
("uparrow")
("upbracefill")
("updownarrow")
("uplus")
("upsilon")
("v")
("varepsilon")
("varphi")
("varpi")
("varrho")
("varsigma")
("vartheta")
("vbadness")
("vdash")
("vdots")
("vec")
("vee")
("vert")
("wedge")
("widehat")
("widetilde")
("widowpenalty")
("wp")
("wr")
("xi")
("xpt")
("zeta")
("~")
)
"List of LaTeX names from lplain.tex, mostly for special symbols.")

(defconst latex-ordinary-commands
 '(
("Alph")
("Roman")
("\\")
("address")
("addtocounter")
("addtolength")
("alph")
("appendix")
("arabic")
("bibliography")
("bigskip")
("centering")
("chapter")
("circle")
("cite")
("cite")
("cleardoublepage")
("clearpage")
("closing") 
("dashbox")
("documentstyle")
("fbox")
("footnotemark")
("footnotetext")
("framebox")
("glossary")
("hline")
("hspace")
("include")
("includeonly")
("index")
("item")
("label")
("line")
("linebreak")
("listoffigures")
("listoftables")
("makebox")
("makeglossary")
("makeindex")
("maketitle")
("mbox")
("medskip")
("multicolumn")
("multiput")
("newcommand")
("newcommand")
("newlength")
("newline")
("newpage")
("newtheorem")
("noalign")
("nofiles")
("noindent")
("nolinebreak")
("nopagebreak")
("normalsize")
("opening")
("oval")
("pagebreak")
("pagenumbering")
("pageref")
("pagestyle")
("parbox")
("protect")
("put")
("raggedleft")
("raggedright")
("raggedright")
("raisebox")
("ref")
("renewcommand")
("roman")
("rule")
("savebox")
("sbox")
("scriptscriptsize")
("scriptsize")
("section")
("setcounter")
("setlength")
("settowidth")
("shortstack")
("shortstack")
("smallskip")
("space")
("stop")
("subsection")
("subsubsection")
("tableentry")
("tableofcontents")
("thicklines")
("thinlines")
("thispagestyle")
("today")
("typein")
("typeout")
("usebox")
("vector")
("verb")
("vline")
("vspace")
; add others that you miss:

)
"List of ordinary LaTeX commands.")

(defconst latex-parameters
  '(
    ("columnsep")     ("footinsertskip")   ("intextsep")    
    ("columnseprule")             ("oddsidemargin")  
    ("columnwidth")              ("textfloatsep")   
    ("evensidemargin")  ("footsep")      ("textheight")    
    ("floatsep")     ("headheight")     ("textwidth")    
    ("headsep")      ("topmargin"))
  "List of LaTex parameters.")

(defconst latex-tabbing-commands

  ;;These commands are defined only within a tabbing environment.
'(
  ("kill")   (">")  ("-") 
  ("pushtab")  ("<")  ("=")
  ("poptab")  ("+") 
  )
"List of LaTeX tabbing commands."
)

(defconst latex-commands
  (append latex-lplain-commands latex-ordinary-commands)
  "List of LaTeX's command (as opposed to environment) names.")
