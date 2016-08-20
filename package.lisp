#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:cl-mpg123-cffi
  (:nicknames #:org.shirakumo.fraf.mpg123.cffi)
  (:use #:cl #:cffi)
  ;; low-level.lisp
  (:shadow :delete :format :open :close :read :position :eq :length)
  (:export
   #:*static*
   #:libmpg123
   #:enc
   #:samplesize
   #:fmt
   #:fmt-rate
   #:fmt-channels
   #:fmt-encoding
   #:size_t
   #:off_t
   #:handle
   #:frameinfo
   #:frameinfo-version
   #:frameinfo-layer
   #:frameinfo-rate
   #:frameinfo-mode
   #:frameinfo-mode-ext
   #:frameinfo-framesize
   #:frameinfo-flags
   #:frameinfo-emphasis
   #:frameinfo-bitrate
   #:frameinfo-abr-rate
   #:frameinfo-vbr
   #:mstring
   #:mstring-p
   #:mstring-size
   #:mstring-fill
   #:text
   #:text-lang
   #:text-id
   #:text-description
   #:text-text
   #:picture
   #:picture-type
   #:picture-description
   #:picture-mime-type
   #:picture-size
   #:picture-data
   #:id3v2
   #:id3v2-version
   #:id3v2-title
   #:id3v2-artist
   #:id3v2-album
   #:id3v2-year
   #:id3v2-genre
   #:id3v2-comment
   #:id3v2-comment-list
   #:id3v2-comments
   #:id3v2-text
   #:id3v2-texts
   #:id3v2-extra
   #:id3v2-extras
   #:id3v2-picture
   #:id3v2-pictures
   #:id3v1
   #:id3v1-tag
   #:id3v1-title
   #:id3v1-artist
   #:id3v1-album
   #:id3v1-year
   #:id3v1-comment
   #:id3v1-genre
   #:pars
   #:id3
   #:new-id3
   #:icy
   #:new-icy
   #:parms
   #:flags
   #:param-rva
   #:feature-set
   #:errors
   #:channelcount
   #:channels
   #:vbr
   #:version
   #:mode
   #:flags
   #:state
   #:text-encoding
   #:id3-enc
   #:id3-pic-type
   #:init
   #:exit
   #:new
   #:delete
   #:param
   #:getparam
   #:feature
   #:plain-strerror
   #:strerror
   #:errcode
   #:decoders
   #:supported-decoders
   #:decoder
   #:current-decoder
   #:rates
   #:encodings
   #:encsize
   #:format-none
   #:format-all
   #:format
   #:format-support
   #:getformat
   #:open
   #:open-fd
   #:open-handle
   #:open-feed
   #:close
   #:read
   #:feed
   #:decode
   #:decode-frame
   #:framebyframe-decode
   #:framebyframe-next
   #:framedata
   #:framepos
   #:tell
   #:tellframe
   #:tell-stream
   #:seek
   #:feedseek
   #:seek-frame
   #:timeframe
   #:index
   #:set-index
   #:position
   #:eq
   #:geteq
   #:reset-eq
   #:volume
   #:volume-change
   #:getvolume
   #:info
   #:safe-buffer
   #:scan
   #:framelength
   #:length
   #:set-filesize
   #:tpf
   #:spf
   #:clip
   #:getstate
   #:init-string
   #:free-string
   #:resize-string
   #:grow-string
   #:copy-string
   #:add-string
   #:add-substring
   #:set-string
   #:set-substring
   #:strlen
   #:chomp-string
   #:enc-from-id3
   #:store-utf8
   #:meta-check
   #:meta-free
   #:id3
   #:icy
   #:icy2utf8
   #:parnew
   #:new-pars
   #:delete-pars
   #:fmt-none
   #:fmt-all
   #:fmt
   #:fmt-support
   #:par
   #:getpar
   #:replace-buffer
   #:outblock
   #:replace-reader
   #:replace-reader-handle))

(defpackage #:cl-mpg123
  (:nicknames #:org.shirakumo.fraf.mpg123)
  (:use #:cl #:cffi)
  (:import-from #:org.shirakumo.fraf.mpg123.cffi #:size_t #:off_t)
  ;; id3-data
  (:export
   #:id3v1-genre
   #:id3v2-genre
   #:id3v2-type)
  ;; metadata.lisp
  (:export
   #:metadata
   #:version
   #:fields
   #:pictures
   #:field
   #:field-text
   #:picture
   #:kind
   #:description
   #:mime-type
   #:data)
  ;; toolkit.lisp
  (:export)
  ;; wrapper.lisp
  (:export
   #:init
   #:exit
   #:file
   #:handle
   #:connected
   #:scanned
   #:buffer
   #:rate
   #:channels
   #:encoding
   #:path
   #:decoder
   #:accepted-format
   #:buffer-size
   #:connect
   #:disconnect
   #:decoders
   #:supported-decoders
   #:supported-rates
   #:supported-encodings
   #:file-format
   #:read-directly
   #:process
   #:decode
   #:decode-frame
   #:sample-position
   #:frame-position
   #:stream-position
   #:seek
   #:time-frame-index
   #:equalizer
   #:reset-equalizer
   #:volume
   #:info
   #:scan
   #:frame-count
   #:sample-count
   #:frame-seconds
   #:frame-samples
   #:track-length
   #:metadata))
