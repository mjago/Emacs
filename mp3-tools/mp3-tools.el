;;; mp3-tools.el --- A simple MP3 Tag Editor for GNU Emacs.

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: Steve Kemp <stevek@epc.co.uk>
;; Keywords: tools, convenience, data
;; Revision: 0.4

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;;  This started out life a small collection of tools related to MP3
;; files, but it has since evolved into a simple MP3 Tag editor.
;;
;;  Currently there is only support for editting the information in
;; one file at a time - but I shall be adding the ability to edit
;; all the files in a directory soon.
;;
;;  In addition to the editor there is a nice function for creating
;; playlists - recursively adding every MP3 file it can find to the
;; list.
;;
;;

;;; Usage:
;;
;; * Put mp3-tools.el in your emacs' site-lisp directory, or your
;;   home directory, and optionally, byte-compile it.
;;
;; * Place the following lines in your .emacs file:
;;
;;  (autoload 'mp3-edit-tag "mp3-tools" "MP3 Tag editor for Emacs" t)
;;  (autoload 'mp3-create-playlist "mp3-tools" "MP3 Playlist generator" t)
;;
;; * To use it after that use:
;;
;;  M-x mp3-tools-edit-tag
;;   To invoke the MP3 Tag editor.
;;
;;  or
;;
;;  M-x mp3-tools-create-playlist
;;   Creates a playlist from all files in, and beneath a named directory.
;;
;;  Steve Kemp <skx@tardis.ed.ac.uk>
;;
;;  http://www.tardis.ed.ac.uk/~skx/
;;


;;; History:
;;
;;  0.1  Created the playlist generation code.
;;
;;  0.2  Initial MP3 Tag editor.
;;
;;  0.3  Allowed MP3 Tag editor to create a tag if one was not already present.
;;
;;  0.4  Split up the code to read the MP3 tag into its own function.
;;       Changed the mp3-file-regexp to cope with upper case extensions.  Doh!
;;       Updated so that the Genre is displayed, and preserved - Not editable yet.
;;


(require 'widget)
(require 'easymenu)
(require 'wid-edit)


;;; Code:

(defvar mp3-tools-file-regexp ".*\\.[Mm][Pp][23]$"
  "A regexp that matches mp3files.")

(defvar mp3-tools-playlist-buffer "playlist.3mu"
  "The name of the buffer to use when creating playlists.")

(defconst mp3-tools-version "mp3-tools v0.4 [Fri Nov 26 10:33:30 1999]"
  "The version description string")


;;;###autoload
(defun mp3-tools-create-playlist (directory)
  "Create a Winamp compatible playlist.
This function will build a playlist for Winamp compatible MP3 players,
recursively descending from the named directory."
  (interactive "DStarting Directory: ")
  ;; Make sure directory has trailing seperator, shouldn't be
  ;; a problem with interactive use.
  (if (string-match (concat ".*[\\/\\\\]$") directory)
      ()
    (setq directory (concat directory "/")))
  (mp3-tools-process-directory directory 'mp3-tools-playlist-function)
  (pop-to-buffer (get-buffer-create mp3-tools-playlist-buffer)))


(defun mp3-tools-process-directory (directory function)
  "Apply a function on all MP3 files in a directory, recursively.
Files are considered to be MP3 files if they match the `mp3-file-regexp'."
  (interactive)
  (message "Entering directory %s" directory)

  ;; Get files
  (let ((file-list (directory-files directory))
	(dir-list nil)
	(file nil)
	(dir nil))
    (setq file-list (remove "." file-list))
    (setq file-list (remove ".." file-list))
    (setq dir-list file-list)
    (message "Directory is %s" directory)
    (message "Working with %s" file-list)
    (while file-list
      (setq file (car file-list))
      (setq file-list (cdr file-list))
      (if (file-directory-p (concat directory file))
	  (message "Directory found in file search %s" (concat directory file))
	(if (string-match mp3-tools-file-regexp file)
	    (funcall function (concat directory file)))))
    (while dir-list
      (setq dir (car dir-list))
      (setq dir-list (cdr dir-list))
      (if (file-directory-p (concat directory dir))
	  (mp3-tools-process-directory (concat directory dir "/" ) function)
	(message "File found in dir search %s" (concat directory  dir))))))


(defun mp3-tools-playlist-function (file)
  "Insert the name of the found mp3file into the playlist buffer."
  (set-buffer (get-buffer-create mp3-tools-playlist-buffer))
  (insert (concat file "\n")))
  

;;;###autoload
(defun mp3-tools-edit-tag (file)
  "Display the text information stored inside an mp3 file.
This also allows you to edit the MP3 Tag information, appending
a new Tag if there isn't one present."
  (interactive "fMP3 File to edit : ")
    
  (let ((songname "")
	(artist   "")
	(albumn   "")
	(year     "")
	(note     "")
	(code     "")
	(info     nil)
	(song-widget nil)
	(artist-widget nil)
	(albumn-widget nil)
	(year-widget nil)
	(note-widget nil)
	(code-widget nil))

    (setq info (mp3-tools-get-tag file))
    
    (if info
	(progn
	  (setq songname (car info))
	  (setq artist (car (cdr info)))
	  (setq albumn (car (cdr (cdr info))))
	  (setq note (car (cdr (cdr (cdr info)))))
	  (setq year (car (cdr (cdr (cdr (cdr info))))))
	  (setq code (car (cdr (cdr (cdr (cdr (cdr info)))))))
	  )
      (message "No MP3 Tag in file")
      )

    ;; Insert information..
    (kill-buffer (get-buffer-create "*MP3 Editor*"))
    (set-buffer (get-buffer-create "*MP3 Editor*"))
    (insert "\n\n\tMP3 Tag Editor\n\n\t")
    (setq song-widget (widget-create 'editable-field
		   :size 30
		   :format "Songname : %v "
		   songname))
    (insert "\n\t")
    (setq artist-widget (widget-create 'editable-field
		   :size 30
		   :format "Artist   : %v "
		   artist))
    (insert "\n\t")
    (setq albumn-widget (widget-create 'editable-field
		   :size 30
		   :format "Albumn   : %v "
		   albumn))
    (insert "\n\t")
    (setq note-widget (widget-create 'editable-field
		   :size 30
		   :format "Note     : %v "
		   note))
    (insert "\n\t")
    (setq year-widget (widget-create 'editable-field
		   :size 4
		   :format "Year     : %v "
		   year))

    (insert "\tGenre: ")
    (insert (mp3-tools-get-genre-description code))
    (insert (concat " (" code ")"))

    (insert "\n\n\t")
    (widget-create 'push-button
		   ;; Whoah!  Scary code.. (I'm sure there must be a better way to do this
		   ;; suggestions appreciated - The problem I'm trying to solve is ensuring
		   ;; the values the user edits are in scope when [s]he clicks on the save
		   ;; button.)
		   :artist artist-widget
		   :code   code
		   :song   song-widget
		   :albumn albumn-widget
		   :year   year-widget
		   :note   note-widget
		   :file   file
		   :notify (lambda (widget &rest ignore)
			     (let ((artist (widget-value (widget-get widget :artist)))
				   (song   (widget-value (widget-get widget :song)))
				   (albumn (widget-value (widget-get widget :albumn)))
				   (year   (widget-value (widget-get widget :year)))
				   (note   (widget-value (widget-get widget :note)))
				   (file   (widget-get widget :file))
				   (code   (widget-get widget :code))
				   )
			       (mp3-tools-save-tag file artist song albumn year note code)))
		   "Save")
    (insert "\t")
    (widget-create 'push-button
		   :notify (lambda (widget &rest ignore)
			(kill-buffer (current-buffer)))
     		 "Quit")

    (pop-to-buffer (get-buffer-create "*MP3 Editor*"))
    (widget-minor-mode)
    (widget-setup)
    ))
    

(defun mp3-tools-get-tag (file)
  "Get MP3tag information from the named file.
This list will be off the form:
  (song-name artist-name albumn-name note year code)."
  (interactive "fMP3 File to interrogate : ")
  (let ((songname "")
	(artist   "")
	(albumn   "")
	(year     "")
	(note     "")
	(code     "")
	(magic    nil))
    (with-temp-buffer
      (insert-file file)
      (goto-char (point-max))
      (backward-char 128)
      (let ((magic (buffer-substring (point) (+ (point) 3))))
	(if (not (string-match "TAG" magic))
	    nil
	  (progn ;; Otherwise search for the info.
	   (setq songname (buffer-substring (+ (point) 3) (+ (point) 33)))
	   (setq artist   (buffer-substring (+ (point) 33) (+ (point) 63)))
	   (setq albumn   (buffer-substring (+ (point) 63) (+ (point) 93)))
	   (setq year     (buffer-substring (+ (point) 93) (+ (point) 97)))
	   (setq note     (buffer-substring (+ (point) 97) (+ (point) 127)))
	   (setq code     (string-to-char (buffer-substring (+ (point) 127) (+ (point) 128))))
	   (list songname artist albumn note year code)))))))


(defun mp3-tools-save-tag (file artist song albumn year note code)
  "Save the specified information to an mp3 file.
The code parameter is ignored currently - this will be fixed in
and updated version."
  (interactive)
  (with-temp-buffer
    (erase-buffer)
    (insert-file file)
    (goto-char (point-max))
    (backward-char 128)
    (let ((magic (buffer-substring (point) (+ (point) 3))))
      (if (string-match "TAG" magic)
	  (delete-char 128)
	(goto-char (point-max))))
    (insert "TAG")
    (mp3-tools-insert-padded-string song 30)
    (mp3-tools-insert-padded-string artist 30)
    (mp3-tools-insert-padded-string albumn 30)
    (mp3-tools-insert-padded-string year 4)
    (mp3-tools-insert-padded-string note 30)
    (insert code)
    (write-region (point-min) (point-max) file)
    (message "Saved MP3 Tag for %s" file)
    (kill-buffer (current-buffer))))


(defun mp3-tools-insert-padded-string (string size)
  "Insert a string into the current buffer, with padding if smaller than the size.
This function will truncate a string that is longer than the allowable size."
  (let ((strlen 0))
    (setq strlen (length string))
    (if (> strlen size)
	(insert (substring string 0 size))
      (if string
	  (insert string))
      (while (< strlen size)
	(insert " ")
	(setq size (- size 1))))))



(defun mp3-tools-get-genre-description( code )
  "Return the string description matching the given genre code.
The descriptions are taken from the standard ones used by id3tool, mp3tag, and Winamp."
  (setq list mp3-tools-genre-codes)
  (let ((cdr nil)
	(entry nil)
	(desc nil)
	(result ""))
    (while list
      (setq entry (car list))
      (setq cde (car entry))
      (setq desc (cdr entry))
      (if (equal cde code)  ;; Really want to terminate this loop here.
	  (setq result desc))
      (setq list (cdr list)))
    result))


;; Definately put this at the back of the file...
(defvar mp3-tools-genre-codes
      (list
       (cons 0  "Blues")
       (cons 1  "Classic Rock")
       (cons 2  "Country")
       (cons 3  "Dance")
       (cons 4  "Disco")
       (cons 5  "Funk")
       (cons 6  "Grunge")
       (cons 7  "Hip-Hop")
       (cons 8  "Jazz")
       (cons 9  "Metal")
       (cons 10  "New Age")
       (cons 11  "Oldies")
       (cons 12  "Other")
       (cons 13  "Pop")
       (cons 14  "R&B")
       (cons 15  "Rap")
       (cons 16  "Reggae")
       (cons 17  "Rock")
       (cons 18  "Techno")
       (cons 19  "Industrial")
       (cons 20  "Alternative")
       (cons 21  "Ska")
       (cons 22  "Death Metal")
       (cons 23  "Pranks")
       (cons 24  "Soundtrack")
       (cons 25  "Euro-Techno")
       (cons 26  "Ambient")
       (cons 27  "Trip-Hop")
       (cons 28  "Vocal")
       (cons 29  "Jazz+Funk")
       (cons 30  "Fusion")
       (cons 31  "Trance")
       (cons 32  "Classical")
       (cons 33  "Instrumental")
       (cons 34  "Acid")
       (cons 35  "House")
       (cons 36  "Game")
       (cons 37  "Sound Clip")
       (cons 38  "Gospel")
       (cons 39  "Noise")
       (cons 40  "Alt. Rock")
       (cons 41  "Bass")
       (cons 42  "Soul")
       (cons 43  "Punk")
       (cons 44  "Space")
       (cons 45  "Meditative")
       (cons 46  "Instrumental Pop")
       (cons 47  "Instrumental Rock")
       (cons 48  "Ethnic")
       (cons 49  "Gothic")
       (cons 50  "Darkwave")
       (cons 51  "Techno-Industrial")
       (cons 52  "Electronic")
       (cons 53  "Pop-Folk")
       (cons 54  "Eurodance")
       (cons 55  "Dream")
       (cons 56  "Southern Rock")
       (cons 57  "Comedy")
       (cons 58  "Cult")
       (cons 59  "Gangsta")
       (cons 60  "Top 40")
       (cons 61  "Christian Rap")
       (cons 62  "Pop/Funk")
       (cons 63  "Jungle")
       (cons 64  "Native US")
       (cons 65  "Cabaret")
       (cons 66  "New Wave")
       (cons 67  "Psychadelic")
       (cons 68  "Rave")
       (cons 69  "Showtunes")
       (cons 70  "Trailer")
       (cons 71  "Lo-Fi")
       (cons 72  "Tribal")
       (cons 73  "Acid Punk")
       (cons 74  "Acid Jazz")
       (cons 75  "Polka")
       (cons 76  "Retro")
       (cons 77  "Musical")
       (cons 78  "Rock & Roll")
       (cons 79  "Hard Rock")
       (cons 80  "Folk")
       (cons 81  "Folk-Rock")
       (cons 82  "National Folk")
       (cons 83  "Swing")
       (cons 84  "Fast Fusion")
       (cons 85  "Bebob")
       (cons 86  "Latin")
       (cons 87  "Revival")
       (cons 88  "Celtic")
       (cons 89  "Bluegrass")
       (cons 91  "Gothic Rock")
       (cons 92  "Progressive Rock")
       (cons 93  "Psychedelic Rock")
       (cons 94  "Symphonic Rock")
       (cons 95  "Slow Rock")
       (cons 96  "Big Band")
       (cons 97  "Chorus")
       (cons 98  "Easy Listening")
       (cons 99  "Acoustic")
       (cons 100  "Humour")
       (cons 101  "Speech")
       (cons 102  "Chanson")
       (cons 103  "Opera")
       (cons 104  "Chamber Music")
       (cons 105  "Sonata")
       (cons 106  "Symphony")
       (cons 107  "Booty Bass")
       (cons 108  "Primus")
       (cons 109  "Porn Groove")
       (cons 110  "Satire")
       (cons 111  "Slow Jam")
       (cons 112  "Club")
       (cons 113  "Tango")
       (cons 114  "Samba")
       (cons 115  "Folklore")
       (cons 116  "Ballad")
       (cons 117  "Power Ballad")
       (cons 118  "Rhytmic Soul")
       (cons 119  "Freestyle")
       (cons 120  "Duet")
       (cons 121  "Punk Rock")
       (cons 122  "Drum Solo")
       (cons 123  "Acapella")
       (cons 124  "Euro-House")
       (cons 125  "Dance Hall")
       (cons 126  "Goa")
       (cons 127  "Drum & Bass")
       (cons 128  "Club-House")
       (cons 129  "Hardcore")
       (cons 130  "Terror")
       (cons 131  "Indie")
       (cons 132  "BritPop")
       (cons 133  "Negerpunk")
       (cons 134  "Polsk Punk")
       (cons 135  "Beat")
       (cons 136  "Christian Gangsta Rap")
       (cons 137  "Heavy Metal")
       (cons 138  "Black Metal")
       (cons 139  "Crossover")
       (cons 140  "Contemporary Christian")
       (cons 141  "Christian Rock")
       (cons 142  "Merengue" )
       (cons 143  "Salsa")
       (cons 144  "Trash Metal")
       (cons 255  "<Error>")
       )
      "This list contains the standard description codes that are used for MP3's.")
	
(provide 'mp3-tools)

;;; mp3-tools.el ends here
