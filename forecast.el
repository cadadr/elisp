;;; forecast.el -- Weather report. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015 Göktuğ Kayaalp
;;
;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: weather, forecast
;; Package-Version: 0.1.0
;; X-URL: http://github.com/cadadr/forecast.el
;; URL: http://github.com/cadadr/forecast.el
;;
;; Permission  is  hereby  granted,  free of  charge,  to  any  person
;; obtaining  a copy  of  this software  and associated  documentation
;; files   (the  "Software"),   to  deal   in  the   Software  without
;; restriction, including without limitation  the rights to use, copy,
;; modify, merge, publish, distribute,  sublicense, and/or sell copies
;; of the  Software, and  to permit  persons to  whom the  Software is
;; furnished to do so, subject to the following conditions:
;;
;; The  above copyright  notice and  this permission  notice shall  be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE  IS PROVIDED  "AS IS", WITHOUT  WARRANTY OF  ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY,   FITNESS    FOR   A   PARTICULAR    PURPOSE   AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT,  TORT OR OTHERWISE, ARISING FROM, OUT  OF OR IN
;; CONNECTION WITH  THE SOFTWARE OR THE  USE OR OTHER DEALINGS  IN THE
;; SOFTWARE.
;;

;;; Commentary:
;;
;; forecast.el generates  a _weather forecast report_  and displays it
;; in a  buffer.  It uses data  from Forecast.io (http://forecast.io),
;; and thus one needs to acquire an  api key from them in order to use
;; this package.  They  allow 1000 requests a day in  their free plan,
;; which should be enough for any user.
;;
;; See Installation section for installation and setup instructions.
;;
;; Report bugs to the Issues page in the package url:
;;
;; https://github.com/cadadr/forecast.el/issues
;;

;;; Installation:
;; 
;; See also «Example configuration».
;; 
;; Put the forecast.el  file somewhere in your path,  then require it.
;; Then set these variables in your configuration:
;; 
;; `forecast-latitude'  Latitude of your location,       float
;; `forecast-longitude' Longitude of your configuration  float
;; `forecast-city'      The name of your city            string
;; `forecast-country'   The name of your country         string
;; `forecast-api-key'   The API key from Forecast.io     string
;; `forecast-language'  Language to use                  symbol
;; `forecast-units'     Unit standard to use             symbol
;; 
;; Only the first  five variables are mandatory.  The  first four have
;; non-sane  defaults, and  if `forecast-api-key'  is absent,  program
;; will not run.
;; 
;; `forecast-city'   and    `forecast-country'   only    for   display
;; purposes. At the moment forecast.el  cannot deduce these names from
;; the latitude and  longitude values, but maybe in future  it will be
;; able to.
;;
;; The first two  variables default to 0.0. The  following two default
;; to "Nil".
;; 
;; The  API  key  can  be obtained  via  registering  oneself  through
;; developer their website:
;;
;; https://developer.forecast.io/
;; 
;; For the rest of variables, see their docstrings (C-h v RET var-name
;; RET).
;;
;; Then on,  you may run  the command  `forecast' to get  the forecast
;; buffer.  The forecast  buffer uses `org-level-*' faces,  so it will
;; look like  your org  files.  It is  called «*Weather  Forecast*» by
;; default,  but if  the  `forecast' command  receives some  universal
;; argument, it will prompt for  an alternative name.  See the section
;; Usage for an explanation of the buffer.
;; 

;;; Example configuration:
;;
;; (setq forecast-latitude 41.168602
;;       forecast-longitude 29.047024
;;       forecast-city "İstanbul"
;;       forecast-country "Türkiye"
;;       forecast-api-key "<deduced>")
;;

;;; Usage:
;;
;; There are 3 keybindings:
;; 
;; g      Refresh the forecast.  Will re-download data.
;; q      Bury the buffer.
;; C-u q  Kill the buffer.
;; 
;; Two  contemporaneous  instances  of  the  program  *will  not*  run
;; correctly, as global state is used to store data.
;; 
;; The buffer is made up of these elements:
;;
;; - At the top, the title, details of location, last update time
;;
;; - Temperature and summary
;;
;; - Apparent temperature (Feels like ...) and detailed summary
;;
;; - Pressure in  ATMs, humidity percentage, wind  speed and direction
;;   from which wind comes. If available, visibility distance.
;;
;; - Summary information for upcoming seven days.
;;
;; - Link to the http://forecast.io.
;;

;;; Contributing:
;;
;; Feel free to send me a pull request.
;;
;; Git repo:
;;
;; https://github.com/cadadr/forecast.el
;;

;;; Changes:
;;
;; v0.1.0, 28 August 2015
;;   - First release.
;;

;;; TODO:
;; 
;; - Automatically find city and country names.
;; - Get location from computer?
;; - I18N?
;; 

;;; Code:

(require 'json)
(require 'cl)
(require 'url)
(require 'subr-x)
(require 'org) ;; Org faces are used.
(require 'button)

(defgroup forecast
  nil
  "Customisations for the forecast.el, the Emacs weather forecasts program."
  :group 'emacs
  :prefix "forecast-")

;;; Variables:
(defvar forecast-city "Nil"
  "The city for which the forecast is given for.

Only for display purposes, variables `forecast-latitude' and
`forecast-longitude' still have to be set correctly.")

(defvar forecast-country "Nil"
  "The country for which the forecast is given for.

Only for display purposes, variables `forecast-latitude' and
`forecast-longitude' still have to be set correctly.")

(defvar forecast-latitude 0.0
  "The latitude of the location for which the forecast shall be
  generated")

(defvar forecast-longitude 0.0
  "The longitude of the location for which the forecast shall be
  generated")

(defvar forecast-api-key ""
  "The API Key from Forecast.io.")

(defvar forecast-api-url "https://api.forecast.io"
  "Base url of the Forecast.io API.

Without the trailing slash")

(defvar forecast--debug nil
  "Whether to surpress error messages.")

(defvar forecast-units 'si
  "Sets the unit standard.

`si'  Standard units.
`us'  US Imperial units.
`ca'  Identical to si, but wind speed in km/h
`uk'  Identical to si, but wind speed is in miles/h, visibility in miles

Any other symbol means that the unit standard is automatically
selected based on the location.")

(defvar forecast-language 'en
  "Language of the forecast.

One of: ar (Arabic), bs (Bosnian), de (German), en (English,
which is the default), es (Spanish), fr (French), it (Italian),
nl (Dutch), pl (Polish), pt (Portuguese), ru (Russian),
sk (Slovak), sv (Swedish), tet (Tetum), tr (Turkish),
uk (Ukrainian), x-pig-latin (Igpay Atinlay), or zh (Chinese).

If not one of these, then `en' is selected.")

(defconst forecast--supported-languages
  '(ar bs de en  es fr it nl pl pt ru sk sv tet tr uk x-pig-latin or zh)
  "List of supported languages.")

(defvar forecast--data nil
  "Forecast data container.")

(defvar forecast--update-time 0
  "The time of last update to the buffer.
As per returned from `current-time'.")

(defvar forecast--buffer nil
  "The Forecast buffer object or name.")

;;; Faces:
(defface forecast-moon-phase
  nil
  "Face for visualisation of moon-phase.

Ideally, set the font family attribute to some font that supports
the characters 01F311-01F318, e.g. Quivira, which can be found at
<http://www.quivira-font.com/>:

\(set-face-attribute 'forecast-moon-phase nil
                     :font \"Quivira\")

On Linux, one can download the Quivira font and put that under
the $HOME/.fonts directory for using the font.  There are not
many fonts that support this character.  There are also the
BabelStone fonts.")

;;; Functions:
(defun assoca (keyseq list)
  "Arbitrary depth multi-level alist query.

KEYSEQ is the list of keys to look up in the LIST.  The first key
from KEYSEQ is looked up in the LIST, then the next key from
KEYSEQ is looked up in the CDR of the return value of that
operation, and so on until all the KEYSEQ is exhausted.  The
resultant value is returned, or nil, in case one or more keys are
not found in the LIST.

Examples:
\(assoca '(a b c)
 '((a . ((b . ((c . e)
               (k . g)))
         (z . q)))
   (r . s)))
 => e

\(assoca '(a t)
 '((a . ((b . ((c . e)
               (k . g)))
         (z . q)))
   (r . s)))
 => nil

\(assoca '(a o t)
 '((a . ((b . ((c . e)
               (k . g)))
         (z . q)))
   (r . s)))
 => nil

\(assoca nil
 '((a . ((b . ((c . e)
               (k . g)))
         (z . q)))
   (r . s)))
 => ((a (b (c . e) (k . g)) (z . q)) (r . s))."
  (let ((ks keyseq)
        (ret list))
    (dolist (k ks ret)
      (setq ret (cdr (assoc k ret))))))

(defun insert-with-props (text &rest props)
  "Insert the given string TEXT and set PROPS lock on it."
  (let ((p1) (p2))
    (setf p1 (point))
    (insert text)
    (setf p2 (point))
    (add-text-properties p1 p2 props)))

(defun insert-format (str &rest fa)
  "Apply format, then insert into the buffer.

STR is the format string.  FA are the arguments to format.  See
`format' for details."
  (insert (apply 'format str fa)))

(defun forecast--get-forecast (callback)
  "Get the forecasts from the Forecast.io API.

CALLBACK is a function of a single argument, WEATHER, the Elisp
representation of the returned JSON from the Forecast.io API."
  (let ((la forecast-latitude)
        (lo forecast-longitude)
        (request-url))
    ;; Make sure LO and LA end up being numbers.
    (when (not (every #'numberp (list la lo)))
      (user-error "Forecast: Latitude and longitude have to be numbers"))
    ;; Check whether we're set for making an API call.
    (when (or (not forecast-api-key)
              (string-empty-p forecast-api-key))
      (user-error "Forecast: `forecast-api-key' not set"))
    (when (or (not forecast-api-key)
              (string-empty-p forecast-api-key))
      (user-error "Forecast: `forecast-api-key' not set"))
    (setf request-url
          (format "%s/forecast/%s/%d,%d?%s"
                  forecast-api-url
                  forecast-api-key
                  la lo
                  (forecast--api-opts)))
    (let ((cb (lambda (status &optional args)
                (ignore args)
                (let ((err (plist-get status :error)))
                  (when err
                    (apply 'signal err)))
                (save-excursion
                  (goto-char (point-min))
                  (re-search-forward "^{")
                  (beginning-of-line)
                  (funcall callback
                           (json-read-from-string
                            (buffer-substring (point)
                                              (point-max))))))))
      (url-retrieve request-url cb nil
                    forecast--debug
                    t ; Inhibit cookies, they are not necessary.
                    ))))

(defun forecast--api-opts ()
  "Generate API options string."
  (let ((f "%s=%s")
        (opts))
    (push (format f "units"
             (case forecast-units
               ((si SI Si) "si")
               ((us US Us) "us")
               ((ca CA Ca) "ca")
               ((uk UK)    "uk2")
               (otherwise  "auto")))
          opts)
    (push (format f "lang"
                  (symbol-name
                    (if (memq forecast-language forecast--supported-languages)
                        forecast-language
                      'en)))
          opts)
    (mapconcat 'identity opts "&")))

(defun forecast--load-data (callback)
  "Load the forecast data into `forecast--data'.

After the data is loaded, the CALLBACK function is called,
passing into it as the argument CBARG.

Arguments LAT, LONG and TIME are identical to those of
`forecast--get-forecast'.

Returns NIL, as it is asynchronous."
  (forecast--get-forecast (lambda (w)
                           (setq forecast--data w)
                           (when forecast--debug
                             (message "Forecast: loaded forecast data."))
                           (funcall callback)
                           (setf forecast--update-time (current-time)))))

(defun forecast--summary ()
  "Return an human-readable summary of the current forecast."
  (assoca '(currently summary) forecast--data))

(defun forecast--temperature ()
  "Return the temperature from the current forecast.

If not available, i.e. not using 'currently, then return the
average of minimum and maximum predicted temperatures."
  (or (assoca '(currently temperature) forecast--data)
      (/ (+ (assoca '(currently temperatureMin) forecast--data)
            (assoca '(currently temperatureMax) forecast--data))
         2)))

(defun forecast--temperature-unit ()
  "Return the temperature unit.

Returns 'F for Fahrenheit, 'C for Centigrade."
  (case forecast-units
    ((US us Us) 'F)
    (otherwise  'C)))

(defun forecast--timezone ()
  "The time zone of the forecast."
  (assoca '(timezone) forecast--data))

(defun forecast--offset ()
  "The offset of the timezone of the forecast from GMT."
  (assoca '(offset) forecast--data))

(defun forecast--temperature-string ()
  "Return a string representing the current temperature.

The temperature, plus the degree sign, plus the unit in capital
letter."
  (format "%.0f%s"
          (forecast--temperature)
          (case (forecast--temperature-unit)
            (C "℃")
            (F "℉")
            (K "K"))))

(defun forecast--pressure (unit)
  "Return pressure in UNIT."
  (let ((p (assoca '(currently pressure) forecast--data)))
    (case unit
      (bar p)
      (atm (forecast--bars-to-atm p))
      (otherwise (error "Forecast: unknown pressure unit: %s" unit)))))

(defun forecast--bars-to-atm (bars)
  "Convert pressure from BARS to ATM."
  (/ bars 1013.25))

(defun forecast--wind-speed ()
  "Return the value for the wind speed."
  (assoca '(currently windSpeed) forecast--data))

(defun forecast--wind-unit ()
  "Find the correct unit for the wind value."
  (case forecast-units
    ((us uk) "mil/h")
    ( ca     "km/h")
    ( si     "m/s")))

(defun forecast--apparent-temperature ()
  "Feels-like temperature, truncated."
  (truncate (or (assoca '(currently apparentTemperature) forecast--data)
                (/ (+ (assoca '(currently apparentTemperatureMin) forecast--data)
                      (assoca '(currently apparentTemperatureMax) forecast--data))
                   2))))

(defun forecast--format-current-time (formats)
  "Return the time for which the forecast is as a formatter time string.

FORMATS is the format string to use.  See `format-time-string'."
  (format-time-string
   formats
   (seconds-to-time (assoca '(currently time) forecast--data))))

(defun forecast--wind-direction ()
  "Calculate and return the direction of current wind."
  (if (zerop (forecast--wind-speed)) ""
      (let ((dir (assoca '(currently windBearing) forecast--data)))
        (upcase (symbol-name (forecast--cardinal-from-degrees dir))))))

(defun forecast--cardinal-from-degrees (d)
  "Turn degrees to one of 4 equivalent cardinal directions or a composed one.

D is a number value, degrees."
  (case (truncate (/ d 22.5))
    (0  'n)
    (1  'n-ne)
    (2  'ne)
    (3  'e-ne)
    (4  'e)
    (5  'e-se)
    (6  'se)
    (7  's-se)
    (8  's)
    (9  's-sw)
    (10 'sw)
    (11 'w-sw)
    (12 'w)
    (13 'w-nw)
    (14 'nw)
    (15 'n-nw)
    (16 'n)
    ;; Wrap around..
    (otherwise (forecast--cardinal-from-degrees (- d 360)))))


(defun forecast--sun-position-graphic ()
  "Visualise the time since the rise of the sun and the time to the set thereof.

E.g.:

Quasi-midday:
>————————☉———————————<
Sunrise:
☉————————————————————<
Sunset:
>————————————————————☉"
  (let* ((today (aref (assoca '(daily data) forecast--data) 0))
         (sunrise (assoca '(sunriseTime) today))
         (sunset  (assoca '(sunsetTime) today))
         (now     (truncate (time-to-seconds (current-time))))
         (daylen  (- sunset sunrise))
         (sunsec  (- now sunrise))
         (wwidth  (window-body-width))
         (graph   (concat ">" (make-string (- wwidth 5) ?—) "<"))
         (glen    (length graph))
         (sun     ?☉)
         (pos    (cond
                  ((>= sunrise now) 0)
                  ((<= sunset now)  (1- glen))
                  (t (1- (/ sunsec (/ daylen wwidth)))))))
    (aset graph pos sun)
    graph))

(defun forecast--detailed-summary ()
  "The more detailed summary of the forecast."
  (assoca '(summary) (aref (assoca '(daily data) forecast--data) 0)))

(defun forecast--visualised-moon-phase ()
  "Visualise the moon phase w/ unicode characters.

See the face `forecast-moon-phase'"
  (let ((mp (assoca '(moonPhase)
                    (aref (assoca '(daily data) forecast--data) 0))))
    (cond ((zerop mp)  "🌑") ; New moon
          ((<  mp .25) "🌒") ; Waxing crescent moon
          ((=  mp .25) "🌓") ; First quarter moon
          ((<  mp .5)  "🌔") ; Waxing gibbous moon
          ((-  mp .5)  "🌕") ; Full moon
          ((<  mp .75) "🌖") ; Waning gibbous moon
          ((=  mp .75) "🌗") ; Last quarter moon
          ((<= mp  1)  "🌘") ; Waning crescent moon
          )))

(defun forecast--humidity ()
  "Humidity percentage."
  (* 100 (assoca '(currently humidity) forecast--data)))

(defun forecast--visibility ()
  "Visibility percentage."
  (let ((v (assoca '(currently visibility) forecast--data)))
    (when v
      (* 100 v))))

(defun forecast--insert-atmosphere-details ()
  "Insert details like pressure, humidity, visibility and wind."
  (insert-format
   "Pressure %1.3f atm; Humidity %.1f%%"
   (forecast--pressure 'atm)
   (forecast--humidity))
  (newline)
  (let ((v (forecast--visibility)))
    (when v
      (insert-format "Visibility %.1f%%" v)))
  (insert-format
   "Wind %s %s, from %s"
   (forecast--wind-speed)
   (forecast--wind-unit)
   (forecast--wind-direction)))

(defun forecast--insert-upcoming ()
        "Forecasts about upcoming 7 days."
        (insert-with-props
          "Upcoming"
          'font-lock-face 'org-level-2)
        (newline)
        (let ((b forecast--data))
          (loop for i from 1 to 7
                do
                (setcdr (assoc 'currently b)
                        (aref (assoca '(daily data) b) i))
                (let ((forecast--data b))
                  (insert-with-props
                    (forecast--format-current-time "%A")
                    'font-lock-face 'org-level-3)
                  (newline)
                  (insert-format
                   "%s, %s"
                   (forecast--temperature-string)
                   (forecast--summary))
                  (newline)
                  (forecast--insert-atmosphere-details)
                  (newline)))))

(defun forecast--insert-io-link ()
  "Insert link to Forecast.io."
  (newline)
  (insert "Powered by")
  (insert " ")
  (insert-text-button
   "forecast.io"
   'follow-link t 'action
   (lambda (b)
     (ignore b)
     (browse-url "http://forecast.io"))))

(defun forecast--insert-location ()
  "Insert location details."
  (insert-with-props
    (format "Forecasts for %s, %s, %s"
            forecast-city
            forecast-country
            (forecast--format-current-time "%F"))
    'font-lock-face 'org-level-5)
  (newline)
  (insert-format "Lat: %f, Long: %f"
                 forecast-latitude
                 forecast-longitude))

(defun forecast--insert-update-time ()
  "Insert the last update time."
  (insert (format-time-string "Last updated %I:%M:%S%p, %F"
                              forecast--update-time))
  (insert-format "; %s, GMT+%d"
                 (forecast--timezone)
                 (forecast--offset)))

(defun forecast--insert-summary ()
  "Insert the summary of today's forecast."
  (insert-with-props
    (format "%s - %s"
            (forecast--temperature-string)
            (forecast--summary))
    'font-lock-face 'org-level-1)
  (newline)
  (insert-with-props
    (format "Feels like %d, %s"
            (forecast--apparent-temperature)
            (forecast--detailed-summary))
    'font-lock-face 'org-level-4))

(defun forecast--insert-sun-moon-graphic ()
  "Insert the combined sun phase and moon phase visualisations."
  (insert-with-props
    (forecast--sun-position-graphic)
    'intangible t)
  (insert-with-props
    (forecast--visualised-moon-phase)
    'font-lock-face 'forecast-moon-phase))

(defun forecast--make-buffer (buffername)
  "(Re)prepare the forecast buffer.

BUFFERNAME is the name of the forecast buffer to use.  Created if
absent."
  (with-current-buffer (get-buffer-create buffername)
    (setq forecast--buffer (current-buffer))
    (when (not buffer-read-only)
      (read-only-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Begin inserting data to the buffer.
      (forecast--insert-location)
      (newline)
      (forecast--insert-update-time)
      (newline)
      (newline)
      (forecast--insert-summary)
      (newline)
      (forecast--insert-sun-moon-graphic)
      (newline)
      (forecast--insert-atmosphere-details)
      (newline)
      (forecast--insert-upcoming)
      (newline)
      (forecast--insert-io-link)

      ;; Finished preparing buffer.
      (goto-char (point-min))
      (forecast-mode))
    ;; Return the prepared buffer.
    (current-buffer)))

(defun forecast (&optional buffername)
  "Bring up the forecast buffer.

TODO If BUFFERNAME is a string, use it as the buffer name.  If
the universal arg is non-zero, prompt user to specify a buffer
name."
  (interactive)
  (forecast--load-data
   (lambda ()
     (let ((buf (forecast--make-buffer (or buffername "*Weather Forecast*"))))
       (switch-to-buffer buf)))))

(defvar forecast-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (prog1 map
      (define-key map "g" 'forecast-refresh)
      (define-key map "q" 'forecast-quit))))

;;; Major mode and keybindings:
(define-derived-mode forecast-mode fundamental-mode
  "Weather Forecast Mode"
  "Major mode for weather forecast buffers."
  (kill-all-local-variables)
  (use-local-map forecast-mode-map)
  (buffer-disable-undo))

(defun forecast-refresh ()
  "Refresh the current forecast buffer."
  (interactive)
  (when (not forecast--buffer)
    (user-error "Run `forecast' instead"))
  (forecast--load-data
   (lambda ()
     (forecast--make-buffer (buffer-name forecast--buffer)))))

(defun forecast-quit (&optional quit)
  "Put away the Forecast buffer.

If QUIT is non-nil or the universal argument is non-nil, kill the
buffer."
  (interactive "^P")
  (quit-window quit))

(provide 'forecast)
;;; forecast.el ends here
