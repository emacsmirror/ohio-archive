;; tex-names.el

(defconst tex-names-version
  "$Id: tex-names.el,v 1.2 90/06/19 18:31:32 sk Exp $"
  "The RCS id of this version of tex-names.")

(provide 'tex-names)

(defconst tex-chardefs
'(
("#")
("$")
("%")
("&")
("AE")
("Delta")
("Gamma")
("Im")
("Lambda")
("Leftarrow")
("Leftrightarrow")
("O")
("OE")
("Omega")
("Phi")
("Pi")
("Psi")
("Re")
("Rightarrow")
("Sigma")
("Theta")
("Upsilon")
("Xi")
("active")
("ae")
("aleph")
("alpha")
("amalg")
("approx")
("ast")
("asymp")
("beta")
("bigcap")
("bigcirc")
("bigcup")
("bigodot")
("bigoplus")
("bigotimes")
("bigsqcup")
("bigtriangledown")
("bigtriangleup")
("biguplus")
("bigvee")
("bigwedge")
("bot")
("braceld")
("bracelu")
("bracerd")
("braceru")
("bullet")
("cap")
("cdot")
("cdotp")
("chi")
("circ")
("clubsuit")
("colon")
("coprod")
("cup")
("dagger")
("dashv")
("ddagger")
("delta")
("diamond")
("diamondsuit")
("div")
("ell")
("emptyset")
("epsilon")
("equiv")
("eta")
("exists")
("flat")
("forall")
("frown")
("gamma")
("ge")
("geq")
("gets")
("gg")
("heartsuit")
("hookleftarrow")
("hookrightarrow")
("i")
("imath")
("in")
("infty")
("int")
("intop")
("iota")
("jmath")
("kappa")
("lambda")
("ldotp")
("le")
("leftarrow")
("leftharpoondown")
("leftharpoonup")
("leftrightarrow")
("leq")
("lhook")
("ll")
("lnot")
("mapsto")
("mapstochar")
("mid")
("mp")
("mu")
("nabla")
("natural")
("nearrow")
("neg")
("ni")
("not")
("nu")
("nwarrow")
("o")
("odot")
("oe")
("oint")
("ointop")
("omega")
("ominus")
("oplus")
("oslash")
("otimes")
("owns")
("parallel")
("partial")
("perp")
("phi")
("pi")
("pm")
("prec")
("preceq")
("prime")
("prod")
("propto")
("psi")
("rho")
("rhook")
("rightarrow")
("rightharpoondown")
("rightharpoonup")
("searrow")
("setminus")
("sharp")
("sigma")
("sim")
("simeq")
("smallint")
("smile")
("spadesuit")
("sqcap")
("sqcup")
("sqsubseteq")
("sqsupseteq")
("ss")
("star")
("subset")
("subseteq")
("succ")
("succeq")
("sum")
("supset")
("supseteq")
("swarrow")
("tau")
("theta")
("times")
("to")
("top")
("triangle")
("triangleleft")
("triangleright")
("uplus")
("upsilon")
("varepsilon")
("varphi")
("varpi")
("varrho")
("varsigma")
("vartheta")
("vdash")
("vee")
("wedge")
("wp")
("wr")
("xi")
("zeta")
)
"List of plain TeX's macro names for characters.")

(defconst tex-defs
'(
("!")
("\"")
("'")
("*")
("+")
(",")
(".")
(";")
("=")
(">")
("AA")
("Arrowvert")
("Big")
("Bigg")
("Biggl")
("Biggm")
("Biggr")
("Bigl")
("Bigm")
("Bigr")
("Downarrow")
("H")
("L")
("Longleftarrow")
("Longleftrightarrow")
("Longrightarrow")
("P")
("Pr")
("Relbar")
("S")
("TeX")
("Uparrow")
("Updownarrow")
("Vert")
("^")
("_")
("`")
("aa")
("acute")
("advancepageno")
("allowbreak")
("angle")
("arccos")
("arcsin")
("arctan")
("arg")
("arrowvert")
("b")
("backslash")
("bar")
("beginsection")
("bf")
("big")
("bigbreak")
("bigg")
("biggl")
("biggm")
("biggr")
("bigl")
("bigm")
("bigr")
("bigskip")
("bmod")
("bordermatrix")
("bowtie")
("brace")
("bracevert")
("brack")
("break")
("breve")
("buildrel")
("bye")
("c")
("cal")
("cases")
("cdots")
("centerline")
("check")
("choose")
("cleartabs")
("columns")
("cong")
("copyright")
("cos")
("cosh")
("cot")
("coth")
("cr")
("csc")
("d")
("dag")
("ddag")
("ddot")
("ddots")
("defaulthyphenchar")
("defaultskewchar")
("deg")
("det")
("dim")
("displaylines")
("dospecials")
("dosupereject")
("dot")
("doteq")
("dotfill")
("dots")
("downarrow")
("downbracefill")
("egroup")
("eject")
("empty")
("endinsert")
("endline")
("enskip")
("enspace")
("eqalign")
("eqalignno")
("exp")
("filbreak")
("fmtname")
("fmtversion")
("folio")
("footnote")
("footnoterule")
("footstrut")
("frenchspacing")
("gcd")
("ge")
("gets")
("goodbreak")
("grave")
("hang")
("hat")
("hbar")
("hglue")
("hidewidth")
("hom")
("hookleftarrow")
("hookrightarrow")
("hphantom")
("hrulefill")
("ialign")
("iff")
("inf")
("int")
("intop")
("it")
("item")
("itemitem")
("iterate")
("joinrel")
("ker")
("l")
("land")
("langle")
("lbrace")
("lbrack")
("lceil")
("ldots")
("le")
("leavevmode")
("leftarrowfill")
("leftline")
("leqalignno")
("letsp")
("lfloor")
("lg")
("lgroup")
("lhook")
("lim")
("liminf")
("limsup")
("line")
("llap")
("lmoustache")
("ln")
("lnot")
("log")
("longleftarrow")
("longleftrightarrow")
("longmapsto")
("longrightarrow")
("loop")
("lor")
("lq")
("magnification")
("magstep")
("magstephalf")
("makefootline")
("makeheadline")
("mapsto")
("mapstochar")
("mathhexbox")
("mathpalette")
("mathstrut")
("matrix")
("max")
("medbreak")
("medskip")
("midinsert")
("min")
("mit")
("models")
("multispan")
("narrower")
("ne")
("negthinspace")
("neq")
("newbox")
("newcount")
("newdimen")
("newfam")
("newhelp")
("newif")
("newinsert")
("newmuskip")
("newread")
("newskip")
("newtoks")
("newwrite")
("next")
("next")
("nobreak")
("noindent")
("nointerlineskip")
("nonfrenchspacing")
("nopagenumbers")
("normalbaselines")
("normalbottom")
("notin")
("null")
("oalign")
("obeyspaces")
("offinterlineskip")
("oint")
("ointop")
("oldstyle")
("ooalign")
("openup")
("overbrace")
("overleftarrow")
("overrightarrow")
("owns")
("pagebody")
("pagecontents")
("pageinsert")
("par")
("phantom")
("plainoutput")
("pmatrix")
("pmod")
("proclaim ")
("qquad")
("quad")
("raggedbottom")
("raggedright")
("rangle")
("rbrace")
("rbrack")
("rceil")
("relbar")
("removelastskip")
("repeat")
("rfloor")
("rgroup")
("rhook")
("rightarrowfill")
("rightleftharpoons")
("rightline")
("rlap")
("rm")
("rmoustache")
("root")
("rq")
("sec")
("settabs")
("showhyphens")
("sin")
("sinh")
("skew")
("sl")
("slash")
("smallbreak")
("smallskip")
("smash")
("space")
("sqrt")
("strut")
("sup")
("supereject")
("surd")
("t")
("tabalign")
("tan")
("tanh")
("textindent")
("thinspace")
("tilde")
("to")
("topinsert")
("tracingall")
("tt")
("ttraggedright")
("u")
("underbar")
("underbrace")
("uparrow")
("upbracefill")
("updownarrow")
("v")
("vdots")
("vec")
("vert")
("vfootnote")
("vglue")
("vphantom")
("widehat")
("widetilde")
("wlog")
("~")
)
"List of plain TeX's macro names.")

(defconst tex-fonts
'(
("bffam")
("fivebf")
("fivei")
("fivei")
("fiverm")
("fivesy")
("fivesy")
("itfam")
("sevenbf")
("seveni")
("seveni")
("sevenrm")
("sevensy")
("sevensy")
("slfam")
("tenbf")
("tenex")
("teni")
("teni")
("tenit")
("tenrm")
("tensl")
("tensy")
("tensy")
("tentt")
("textfont")
("ttfam")
)
"List of plain TeX's macro names for fonts.")

(defconst tex-parameters 
'(
("abovedisplayshortskip")
("abovedisplayskip")
("adjdemerits")
("baselineskip")
("belowdisplayshortskip")
("belowdisplayskip")
("bigskipamount ")
("binoppenalty")
("boxmaxdepth")
("brokenpenalty")
("clubpenalty")
("day")
("defaulthyphenchar")
("defaultskewchar")
("delimiterfactor")
("delimitershortfall")
("displayindent")
("displaywidowpenalty")
("displaywidth")
("doublehyphendemerits")
("endlinechar")
("escapechar")
("exhyphenpenalty")
("fam")
("finalhyphendemerits")
("floatingpenalty")
("globaldefs")
("hangafter")
("hangindent")
("hbadness")
("hfuzz")
("hoffset")
("hsize")
("hyphenpenalty")
("interdisplaylinepenalty")
("interfootnotelinepenalty")
("interlinepenalty")
("jot ")
("leftskip")
("linepenalty")
("lineskip")
("lineskiplimit")
("looseness")
("mag")
("magstephalf")
("mathsurround")
("maxdeadcycles")
("maxdepth")
("medmuskip")
("medskipamount")
("month")
("newlinechar")
("normalbaselineskip")
("normallineskip")
("normallineskiplimit")
("nulldelimiterspace")
("outputpenalty")
("overfullrule")
("parfillskip")
("parindent")
("parskip")
("pausing")
("postdisplaypenalty")
("predisplaypenalty")
("predisplaysize")
("pretolerance")
("relpenalty")
("rightskip")
("scriptspace")
("showboxbreadth")
("showboxdepth")
("smallskipamount")
("spaceskip")
("splitmaxdepth")
("splittopskip")
("tabskip")
("thickmuskip")
("thinmuskip")
("time")
("tolerance")
("topskip")
("tracingcommands")
("tracinglostchars")
("tracingmacros")
("tracingonline")
("tracingoutput")
("tracingpages")
("tracingparagraphs")
("tracingrestores")
("tracingstats")
("uchyph")
("vbadness")
("vfuzz")
("voffset")
("vsize")
("widowpenalty")
("xspaceskip")
("year")
("magstep")
)
"List of plain TeX's macro names for parameters.")

(defconst tex-names 
  (append tex-chardefs tex-defs tex-fonts tex-parameters)
  "List of all plain TeX's macro names: macros, characters, parameters, fonts.")
