Template buffer-name Function
:begin
(buffer-name)
:end

Template buffer-file-name Function
:begin
(buffer-file-name)
:end

Template current-time-string Function
:begin
(current-time-string)
:end

Template date Function
:begin
(concat "<monthnumber>" "/" "<day>" "/" "<year>")
:end

Template day Function
:begin
(let ((str (current-time-string)))
  (string-match "[0-9]+" str)
  (substring str (match-beginning 0) (match-end 0))
)
:end

Template file-name-directory Function
:begin
(file-name-directory (buffer-file-name))
:end

Template file-name-nondirectory Function
:begin
(file-name-nondirectory (buffer-file-name))
:end

Template month Function
:begin
(let ((str (current-time-string))
      prefix)
  (setq prefix (substring str 4 7))
  (cond
    ((equal prefix "Jan")
      "January")
    ((equal prefix "Feb")
      "February")
    ((equal prefix "Mar")
      "March")
    ((equal prefix "Apr")
      "April")
    ((equal prefix "May")
      "May")
    ((equal prefix "Jun")
      "June")
    ((equal prefix "Jul")
      "July")
    ((equal prefix "Aug")
      "August")
    ((equal prefix "Sep")
      "September")
    ((equal prefix "Oct")
      "October")
    ((equal prefix "Nov")
      "November")
    ((equal prefix "Dec")
      "December")
  )
)
:end

Template monthnumber Function
:begin
(let ((str (current-time-string))
      prefix)
  (setq prefix (substring str 4 7))
  (cond
    ((equal prefix "Jan")
      "1")
    ((equal prefix "Feb")
      "2")
    ((equal prefix "Mar")
      "3")
    ((equal prefix "Apr")
      "4")
    ((equal prefix "May")
      "5")
    ((equal prefix "Jun")
      "6")
    ((equal prefix "Jul")
      "7")
    ((equal prefix "Aug")
      "8")
    ((equal prefix "Sep")
      "9")
    ((equal prefix "Oct")
      "10")
    ((equal prefix "Nov")
      "11")
    ((equal prefix "Dec")
      "12")
  )
)
:end

Template today Function
:begin
(concat "<month>" " " "<day>" ", 19" "<year>")
:end

Template time Function
:begin
(let ((str (current-time-string)))
  (string-match "[0-9][0-9]:[0-9][0-9]" str)
  (substring str (match-beginning 0) (match-end 0))
)
:end

Template user-full-name Function
:begin
(user-full-name)
:end

Template weekday Function
:begin
(let ((str (current-time-string))
      prefix)
  (setq prefix (substring str 0 3))
  (cond
    ((equal prefix "Sun")
      "Sunday")
    ((equal prefix "Mon")
      "Monday")
    ((equal prefix "Tue")
      "Tuesday")
    ((equal prefix "Wed")
      "Wednesday")
    ((equal prefix "Thu")
      "Thursday")
    ((equal prefix "Fri")
      "Friday")
    ((equal prefix "Sat")
      "Saturday")
  )
)
:end

Template year Function
:begin
(substring (current-time-string) -2)
:end


Local Variables:
tpl-begin-template-definition:"^Template"
tpl-begin-template-body:"^:begin"
tpl-end-template-body:"^:end"
end:
