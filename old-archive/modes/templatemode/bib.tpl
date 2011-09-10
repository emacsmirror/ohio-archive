Template citation Selection
:begin
article:
book:
booklet:
inbook:
incollection:
inproceedings:
manual:
mastersthesis:
misc:
phdthesis:
proceedings:
techreport:
unpublished:
:end

Template keywords Selection
:begin
address:
annote:
author:
booktitle:
chapter:
edition:
editor:
how-published:
institution:
journal:
key:
month-published:
note:
number:
organization:
pages:
publisher:
school:
series:
title:
volume:
year:
:end

Template article Sequence
:begin
@Article{<text:name><2digits:year>
,Key = "<text:name>"
,Author = "<text:name><text:authors>"
,Title = "<text:title>"
,Journal = "<text:journal>"
,Year = "19<2digits:year>"
<#month-published>
<#number>
<#pages>
<#volume>
<#note>
<#annote>
}
:end

Template book Sequence
:begin
@Book{<text:name><2digits:year>
,Key = "<text:name>"
,Author = "<text:name><text:authors>"
,Title = "<text:title>"
,Publisher = "<text:publisher>"
,Year = "19<2digits:year>"
<#address>
<#series>
<#volume>
<#note>
<#annote>
}
:end

Template booklet Sequence
:begin
@Booklet{<text:name><2digits:year>
,Key = "<text:name>"
,Author = "<text:name><text:authors>"
,Title = "<text:title>"
,Year = "19<2digits:year>"
<#address>
<#how-published>
<#note>
<#annote>
}
:end

Template inbook Sequence
:begin
@InBook{<text:name><2digits:year>
,Key = "<text:name>"
,Author = "<text:name><text:authors>"
,Title = "<text:title>"
,BookTitle = "<text:booktitle>"
,Publisher = "<text:publisher>"
,Year = "19<2digits:year>"
<#address>
<#chapter>
<#pages>
<#series>
<#volume>
<#note>
<#annote>
}
:end

Template incollection Sequence
:begin
@InCollection{<text:name><2digits:year>
,Key = "<text:name>"
,Author = "<text:name><text:authors>"
,Title = "<text:title>"
,BookTitle = "<text:booktitle>"
,Publisher = "<text:publisher>"
,Year = "19<2digits:year>"
<#address>
<#chapter>
<#editor>
<#pages>
<#series>
<#volume>
<#note>
<#annote>
}
:end

Template inproceedings Sequence
:begin
@InProceedings{<text:name><2digits:year>
,Key = "<text:name>"
,Author = "<text:name><text:authors>"
,Title = "<text:title>"
,BookTitle = "<text:booktitle>"
,Publisher = "<text:publisher>"
,Year = "19<2digits:year>"
<#address>
<#editor>
<#month-published>
<#pages>
<#note>
<#annote>
}
:end

Template manual Sequence
:begin
@Manual{<text:name><2digits:year>
,Key = "<text:name>"
,Title = "<text:title>"
,Year = "19<2digits:year>"
<#address>
<#author>
<#edition>
<#organization>
<#note>
<#annote>
}
:end

Template mastersthesis Sequence
:begin
@MastersThesis{<text:name><2digits:year>
,Key = "<text:name>"
,Author = "<text:name><text:authors>"
,Title = "<text:title>"
,School = "<text:school>"
,Year = "19<2digits:year>"
<#month-published>
<#note>
<#annote>
}
:end

Template misc Sequence
:begin
@Misc{<text:name><2digits:year>
,Key = "<text:name>"
<#author>
<#how-published>
<#title>
<#note>
<#annote>
}
:end

Template phdthesis Sequence
:begin
@PhDThesis{<text:name><2digits:year>
,Key = "<text:name>"
,Author = "<text:name><text:authors>"
,Title = "<text:title>"
,School = "<text:school>"
,Year = "19<2digits:year>"
<#month-published>
<#note>
<#annote>
}
:end

Template proceedings Sequence
:begin
@Proceedings{<text:name><2digits:year>
,Key = "<text:name>"
,Editor = "<text:name><text:editors>"
,Title = "<text:title>"
,Publisher = "<text:publisher>"
,Year = "19<2digits:year>"
<#address>
<#note>
<#annote>
}
:end

Template techreport Sequence
:begin
@TechReport{<text:name><2digits:year>
,Key = "<text:name>"
,Author = "<text:name><text:authors>"
,Title = "<text:title>"
,Institution = "<text:institution>"
,Year = "19<2digits:year>"
<#month-published>
<#number>
<#note>
<#annote>
}
:end

Template unpublished Sequence
:begin
@Unpublished{<text:name><2digits:year>
,Key = "<text:name>"
,Author = "<text:name><text:authors>"
,Title = "<text:title>"
<#year>
<#note>
<#annote>
}
:end

Template address Sequence
:begin
, Address = "<text:address>"
:end

Template annote Sequence
:begin
, Annote = "<textlong:annote>"
:end

Template author Sequence
:begin
, Author = "<text:name><text:authors>"
:end

Template booktitle Sequence
:begin
, BookTitle = "<text:booktitle>"
:end

Template chapter Sequence
:begin
, Chapter = "<text:chapter>"
:end

Template edition Sequence
:begin
, Edition = "<text:edition>"
:end

Template editor Sequence
:begin
, Editor = "<text:editor>"
:end

Template how-published Sequence
:begin
, HowPublished = "<text:how-published>"
:end

Template institution Sequence
:begin
, Institution = "<text:institution>"
:end

Template journal Sequence
:begin
, Journal = "<text:journal>"
:end

Template key Sequence
:begin
, Key = "<text:key>"
:end

Template month-published Sequence
:begin
, Month = "<text:month-published>"
:end

Template note Sequence
:begin
, Note = "<textlong:note>"
:end

Template number Sequence
:begin
, Number = "<text:number>"
:end

Template organization Sequence
:begin
, Organization = "<text:organization>"
:end

Template pages Sequence
:begin
, Pages = "<text:pages>"
:end

Template publisher Sequence
:begin
, Publisher = "<text:publisher>"
:end

Template school Sequence
:begin
, School = "<text:school>"
:end

Template series Sequence
:begin
, Series = "<text:series>"
:end

Template title Sequence
:begin
, Title = "<text:title>"
:end

Template volume Sequence
:begin
, Volume = "<text:volume>"
:end

Template year Sequence
:begin
, Year = "19<2digits:year>"
:end

Template 2digits Lexical
:begin
[0-9][0-9]
:end


Local Variables:
tpl-begin-template-definition:"^Template"
tpl-begin-template-body:"^:begin"
tpl-end-template-body:"^:end"
end:
