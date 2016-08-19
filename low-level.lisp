#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mpg123.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(define-foreign-library libmpg123
  (:darwin (:or "libmpg123.dylib" "libmpg123.so"
                #+X86 "mac32-libmpg123.dylib"
                #+X86-64 "mac64-libmpg123.dylib"))
  (:unix (:or "libmpg123.so"
              #+X86 "lin32-libmpg123.so"
              #+X86-64 "lin64-libmpg123.so"))
  (:windows (:or "mpg123.dll"
                 #+X86 "win32-libmpg123.dll"
                 #+X86-64 "win64-libmpg123.dll"))
  (t (:default "mpg123")))

(use-foreign-library libmpg123)

;;; fmt123.h
(defcenum enc
  (:8           #x000F)
  (:16          #x0040)
  (:24          #x4000)
  (:32          #x0100)
  (:signed      #x0080)
  (:float       #x0E00)
  (:signed-16   #x00D0)                 ; 16 | signed | 0x10
  (:unsigned-16 #x0060)                 ; 16 | 0x20
  (:unsigned-8  #x0001)
  (:signed-8    #x0082)                 ; signed | 0x02
  (:ulaw-8      #x0004)
  (:alaw-8      #x0008)
  (:signed-32   #x1180)                 ; 32 | signed | 0x1000
  (:unsigned-32 #x2100)                 ; 32 | 0x2000
  (:signed-24   #x5080)                 ; 24 | signed | 0x1000
  (:unsigned-24 #x6000)                 ; 24 | 0x2000
  (:float-32    #x0200)
  (:float-64    #x0400)
  (:any         #x77FF))                ;  signed-16 | unsigned-16 | unsigned-8 | signed-8
                                        ;| ulaw-8    | alaw-8      | signed-32  | unsigned-32
                                        ;| signed-24 | unsigned-24 | float-32   | float-64

(declaim (inline samplesize))
(defun samplesize (enc)
  (cond ((/= 0 (logand enc (foreign-enum-value 'enc :8)))
         1)
        ((/= 0 (logand enc (foreign-enum-value 'enc :16)))
         2)
        ((/= 0 (logand enc (foreign-enum-value 'enc :24)))
         3)
        ((or (/= 0 (logand enc (foreign-enum-value 'enc :32)))
             (= enc (foreign-enum-value 'enc :float-32)))
         4)
        ((= enc (foreign-enum-value 'enc :float-64))
         8)
        (T
         0)))

(defcstruct (fmt :class fmt :conc-name fmt-)
  (rate :long)
  (channels :int)
  (encoding :int))

;;; mpg123.h
(defctype size_t :unsigned-int)
(defctype off_t :int)

(defconstant ID3     #x03)
(defconstant NEW-ID3 #x01)
(defconstant ICY     #x0C)
(defconstant NEW-ICY #x04)

(defcenum parms
  (:verbose 0)
  :flags
  :add-flags
  :force-rate
  :down-sample
  :rva
  :downspeed
  :upspeed
  :start-frame
  :decode-frames
  :icy-interval
  :outscale
  :timeout
  :remove-flags
  :resync-limit
  :index-size
  :preframes
  :feedpool
  :feedbuffer)

(defcenum flags
  (:force-mono          #x00007)
  (:mono-left           #x00001)
  (:mono-right          #x00002)
  (:mono-mix            #x00004)
  (:force-stereo        #x00008)
  (:force-8bit          #x00010)
  (:quiet               #x00020)
  (:fapless             #x00040)
  (:no-resync           #x00080)
  (:seekbuffer          #x00100)
  (:fuzzy               #x00200)
  (:force-float         #x00400)
  (:plain-id3text       #x00800)
  (:ignore-streamlength #x01000)
  (:skip-id3v2          #x02000)
  (:ignore-infoframe    #x04000)
  (:auto-resample       #x08000)
  (:picture             #x10000))

(defcenum param-rva
  (:off 0)
  (:mix 1)
  (:album 2)
  (:max 2))

(defcenum feature-set
  (:abi-utf8open 0)
  :output-8bit
  :output-16bit
  :output-32bit
  :index
  :parse-id3v2
  :decode-layer1
  :decode-layer2
  :decode-layer3
  :decode-accurate
  :decode-downsample
  :decode-ntom
  :parse-icy
  :timeout-read
  :equalizer)

(defcenum errors
  (:done -12)
  (:new-format -11)
  (:need-more -10)
  (:err -1)
  (:ok 0)
  :bad-outformat
  :bad-channel
  :bad-rate
  :err-16to8table
  :bad-param
  :bad-buffer
  :out-of-mem
  :not-initialized
  :bad-decoder
  :bad-handle
  :no-buffers
  :bad-rva
  :no-gapless
  :no-space
  :bad-types
  :bad-band
  :err-null
  :err-reader
  :no-seek-from-end
  :bad-whence
  :no-timeout
  :bad-file
  :no-seek
  :no-reader
  :bad-pars
  :bad-index-par
  :out-of-sync
  :resync-fail
  :no-8bit
  :bad-align
  :null-buffer
  :no-relseek
  :null-pointer
  :bad-key
  :no-index
  :index-fail
  :bad-decoder-setup
  :missing-feature
  :bad-value
  :lseek-failed
  :bad-custom-io
  :lfs-overflow
  :int-overflow)

(defcenum channelcount
  (:error       0)
  (:mono        1)
  (:stereo      2)
  (:stereo-mono 3))

(defcenum channels
  (:left  #x01)
  (:right #x02)
  (:lr    #x02))

(defcenum vbr
  (:cbr 0)
  :vbr
  :abr)

(defcenum version
  (:1.0 0)
  :2.0
  :2.5)

(defcenum mode
  (:stero 0)
  :join
  :dual
  :mono)

(defcenum flags
  (:crc       #x01)
  (:copyright #x02)
  (:private   #x04)
  (:original  #x08))

(defcenum state
  (:accurate 1)
  :bufferfill
  :frankenstein
  :fresh-decoder)

(defcenum text-encoding
  (:unknown  0)
  (:utf8     1)
  (:latin1   2)
  (:icy      3)
  (:cp1252   4)
  (:utf16    5)
  (:utf16bom 6)
  (:utf16be  7)
  (:max      7))

(defcenum id3-enc
  (:latin1   0)
  (:utf16bom 1)
  (:utf16be  2)
  (:utf8     3)
  (:max      3))

(defcenum id3-pic-type
  (:other           0)
  (:icon            1)
  (:other-icon      2)
  (:front-cover     3)
  (:back-cover      4)
  (:leaflet         5)
  (:media           6)
  (:lead            7)
  (:artist          8)
  (:conductor       9)
  (:orchestra      10)
  (:composer       11)
  (:lyricist       12)
  (:location       13)
  (:recording      14)
  (:performance    15)
  (:video          16)
  (:fish           17)
  (:illustration   18)
  (:artist-logo    19)
  (:publisher-logo 20))

(defcstruct (handle :class handle))

(defcstruct (frameinfo :class frameinfo :conc-name frameinfo-)
  (version version)
  (layer :int)
  (rate :long)
  (mode mode)
  (mode-ext :int)
  (framesize :int)
  (flags :int)
  (emphasis :int)
  (bitrate :int)
  (abr-rate :int)
  (vbr vbr))

(defcstruct (mstring :class mstring :conc-name mstring-)
  (p (:pointer :char))
  (size size_t)
  (fill size_t))

(defcstruct (text :class text :conc-name text-)
  (lang :char :count 3)
  (id :char :count 4)
  (description (:struct mstring))
  (text (:struct mstring)))

(defcstruct (picture :class picture :conc-name picture-)
  (type id3-pic-type)
  (description (:struct mstring))
  (mime-type (:struct mstring))
  (size size_t)
  (data (:pointer :unsigned-char)))

(defcstruct (id3v2 :class id3v2 :conc-name id3v2-)
  (version :unsigned-char)
  (title (:pointer (:struct mstring)))
  (artist (:pointer (:struct mstring)))
  (album (:pointer (:struct mstring)))
  (year (:pointer (:struct mstring)))
  (genre (:pointer (:struct mstring)))
  (comment (:pointer (:struct mstring)))
  (comment-list (:pointer (:struct text)))
  (comments size_t)
  (text (:pointer (:struct text)))
  (texts size_t)
  (extra (:pointer (:struct text)))
  (extras size_t)
  (picture (:pointer (:struct picture)))
  (pictures size_t))

(defcstruct (id3v1 :class id3v1 :conc-name id3v1-)
  (tag :char :count 3)
  (title :char :count 30)
  (artist :char :count 30)
  (album :char :count 30)
  (year :char :count 4)
  (comment :char :count 30)
  (genre :unsigned-char))

(defcstruct (pars :class pars))

(defcfun (init "mpg123_init") errors)

(defcfun (exit "mpg123_exit") :void)

(defcfun (new "mpg123_new") :pointer
  (decoder :string)
  (error (:pointer :int)))

(defcfun (delete "mpg123_delete") :void
  (handle :pointer))

(defcfun (param "mpg123_param") errors
  (handle :pointer)
  (type parms)
  (value :long)
  (fvalue :double))

(defcfun (getparam "mpg123_getparam") errors
  (handle :pointer)
  (type parms)
  (value (:pointer :long))
  (fvalue (:pointer :double)))

(defcfun (feature "mpg123_feature") :bool
  (key feature-set))

(defcfun (plain-strerror "mpg123_plain_strerror") :string
  (errcode errors))

(defcfun (strerror "mpg123_sterror") :string
  (handle :pointer))

(defcfun (errcode "mpg123_errcode") :int
  (handle :pointer))

(defcfun (decoders "mpg123_decoders") (:pointer :string))

(defcfun (supported-decoders "mpg123_supported_decoders") (:pointer :string))

(defcfun (decoder "mpg123_decoder") errors
  (handle :pointer)
  (name :string))

(defcfun (current-decoder "mpg123_current_decoder") :string
  (handle :pointer))

(defcfun (rates "mpg123_rates") :void
  (list (:pointer (:pointer :long)))
  (number (:pointer size_t)))

(defcfun (encodings "mpg123_encodings") :void
  (list (:pointer (:pointer :int)))
  (number (:pointer size_t)))

(defcfun (encsize "mpg123_encsize") :int
  (encoding :int))

(defcfun (format-none "mpg123_format_none") errors
  (handle :pointer))

(defcfun (format-all "mpg123_format_all") errors
  (handle :pointer))

(defcfun (format "mpg123_format") errors
  (handle :pointer)
  (rate :long)
  (channels channelcount)
  (encodings enc))

(defcfun (format-support "mpg123_format_support") channelcount
  (handle :pointer)
  (rate :long)
  (encoding enc))

(defcfun (getformat "mpg123_getformat") errors
  (handle :pointer)
  (rate (:pointer :long))
  (channels (:pointer channelcount))
  (encoding (:pointer enc)))

(defcfun (open "mpg123_open") errors
  (handle :pointer)
  (path :string))

(defcfun (open-fd "mpg123_open_fd") errors
  (handle :pointer)
  (fd :int))

(defcfun (open-handle "mpg123_open_handle") errors
  (handle :pointer)
  (iohandle :pointer))

(defcfun (open-feed "mpg123_open_feed") errors
  (handle :pointer))

(defcfun (close "mpg123_close") errors
  (handle :pointer))

(defcfun (read "mpg123_read") errors
  (handle :pointer)
  (outmemory (:pointer :unsigned-char))
  (outmemsize size_t)
  (done (:pointer size_t)))

(defcfun (feed "mpg123_feed") errors
  (handle :pointer)
  (in (:pointer :unsigned-char))
  (size size_t))

(defcfun (decode "mpg123_decode") errors
  (handle :pointer)
  (inmemory (:pointer :unsigned-char))
  (inmemsize size_t)
  (outmemory (:pointer :unsigned-char))
  (outmemsize size_t)
  (done size_t))

(defcfun (decode-frame "mpg123_decode_frame") errors
  (handle :pointer)
  (num (:pointer off_t))
  (audio (:pointer (:pointer :unsigned-char)))
  (bytes (:pointer size_t)))

(defcfun (framebyframe-decode "mpg123_framebyframe_decode") errors
  (handle :pointer)
  (num (:pointer off_t))
  (audio (:pointer (:pointer :unsigned-char)))
  (bytes (:pointer size_t)))

(defcfun (framebyframe-next "mpg123_framebyframe_next") errors
  (handle :pointer))

(defcfun (framedata "mpg123_framedata") errors
  (handle :pointer)
  (header (:pointer :unsigned-long))
  (bodydata (:pointer (:pointer :unsigned-char)))
  (bodybytes (:pointer size_t)))

(defcfun (framepos "mpg123_framepos") off_t
  (handle :pointer))

(defcfun (tell "mpg123_tell") off_t
  (handle :pointer))

(defcfun (tellframe "mpg123_tellframe") off_t
  (handle :pointer))

(defcfun (tell-stream "mpg123_tell_stream") off_t
  (handle :pointer))

(defcfun (seek "mpg123_seek") off_t
  (handle :pointer)
  (sampleoff off_t)
  (whence :int))

(defcfun (feedseek "mpg123_feedseek") off_t
  (handle :pointer)
  (sampleoff off_t)
  (whence :int)
  (input-offset (:pointer off_t)))

(defcfun (seek-frame "mpg123_seek_frame") off_t
  (handle :pointer)
  (frameoff off_t)
  (whence :int))

(defcfun (timeframe "mpg123_timeframe") off_t
  (handle :pointer)
  (sec :double))

(defcfun (index "mpg123_index") errors
  (handle :pointer)
  (offsets (:pointer (:pointer off_t)))
  (step (:pointer off_t))
  (fill (:pointer size_t)))

(defcfun (set-index "mpg123_set_index") errors
  (handle :pointer)
  (offsets (:pointer off_t))
  (step off_t)
  (fill size_t))

;; DON'T USE!
(defcfun (position "mpg123_position") errors
  (handle :pointer)
  (frame-offset off_t)
  (buffered-bytes off_t)
  (current-frame (:pointer off_t))
  (frames-left (:pointer off_t))
  (current-seconds (:pointer :double))
  (seconds-left (:pointer :double)))

(defcfun (eq "mpg123_eq") errors
  (handle :pointer)
  (channel channels)
  (band :int)
  (val :double))

(defcfun (geteq "mpg123_geteq") :double
  (handle :pointer)
  (channel channels)
  (band :int))

(defcfun (reset-eq "mpg123_reset_eq") errors
  (handle :pointer))

(defcfun (volume "mpg123_volume") errors
  (handle :pointer)
  (vol :double))

(defcfun (volume-change "mpg123_volume_change") errors
  (handle :pointer)
  (change :double))

(defcfun (getvolume "mpg123_getvolume") errors
  (handle :pointer)
  (base (:pointer :double))
  (really (:pointer :double))
  (rva-db (:pointer :double)))

(defcfun (info "mpg123_info") errors
  (handle :pointer)
  (frameinfo :pointer))

(defcfun (safe-buffer "mpg123_safe_buffer") size_t)

(defcfun (scan "mpg123_scan") errors
  (handle :pointer))

(defcfun (framelength "mpg123_framelength") off_t
  (handle :pointer))

(defcfun (length "mpg123_length") off_t
  (handle :pointer))

(defcfun (set-filesize "mpg123_set_filesize") errors
  (handle :pointer)
  (size off_t))

(defcfun (tpf "mpg123_tpf") :double
  (handle :pointer))

(defcfun (spf "mpg123_spf") :int
  (handle :pointer))

(defcfun (clip "mpg123_clip") :long
  (handle :pointer))

(defcfun (getstate "mpg123_getstate") errors
  (handle :pointer)
  (key state)
  (val (:pointer :long))
  (fval (:pointer :double)))

(defcfun (init-string "mpg123_init_string") :void
  (mstring :pointer))

(defcfun (free-string "mpg123_free_string") :void
  (mstring :pointer))

(defcfun (resize-string "mpg123_resize_string") :bool
  (mstring :pointer)
  (news size_t))

(defcfun (grow-string "mpg123_grow_string") :bool
  (mstring :pointer)
  (news size_t))

(defcfun (copy-string "mpg123_copy_string") :bool
  (from :pointer)
  (to :pointer))

(defcfun (add-string "mpg123_add_string") :bool
  (mstring :pointer)
  (stuff :string))

(defcfun (add-substring "mpg123_add_substring") :bool
  (mstring :pointer)
  (stuff :string)
  (from size_t)
  (count size_t))

(defcfun (set-string "mpg123_set_string") :bool
  (mstring :pointer)
  (stuff :string))

(defcfun (set-substring "mpg123_set_substring") :bool
  (mstring :pointer)
  (stuff :string)
  (from size_t)
  (count size_t))

(defcfun (strlen "mpg123_strlen") size_t
  (mstring :pointer)
  (utf8 :int))

(defcfun (chomp-string "mpg123_comp_string") :bool
  (mstring :pointer))

(defcfun (enc-from-id3 "mpg123_enc_from_id3") text-encoding
  (id3-enc-byte :unsigned-char))

(defcfun (store-utf8 "mpg123_store_utf8") :bool
  (mstring :pointer)
  (enc text-encoding)
  (source (:pointer :unsigned-char))
  (source-size size_t))

(defcfun (meta-check "mpg123_meta_check") :int
  (handle :pointer))

(defcfun (meta-free "mpg123_meta_free") :void
  (handle :pointer))

(defcfun (id3 "mpg123_id3") errors
  (handle :pointer)
  (v1 :pointer)
  (v2 :pointer))

(defcfun (icy "mpg123_icy") errors
  (handle :pointer)
  (icy-meta (:pointer (:pointer :char))))

(defcfun (icy2utf8 "mpg123_icy2utf8") :string
  (icy_text (:pointer :char)))

(defcfun (parnew "mpg123_parnew") :pointer
  (pars :pointer)
  (decoder :string)
  (error (:pointer :int)))

(defcfun (new-pars "mpg123_new_pars") :pointer
  (error (:pointer :int)))

(defcfun (delete-pars "mpg123_delete_pars") :void
  (pars :pointer))

(defcfun (fmt-none "mpg123_fmt_none") errors
  (pars :pointer))

(defcfun (fmt-all "mpg123_fmt_all") errors
  (pars :pointer))

(defcfun (fmt "mpg123_fmt") errors
  (pars :pointer)
  (rate :long)
  (channels :int)
  (encodings :int))

(defcfun (fmt-support "mpg123_fmt_support") channelcount
  (pars :pointer)
  (rate :long)
  (encoding :int))

(defcfun (par "mgp123_par") errors
  (pars :pointer)
  (type parms)
  (value :long)
  (fvalue :double))

(defcfun (getpar "mpg123_getpar") errors
  (pars :pointer)
  (type parms)
  (value (:pointer :long))
  (fvalue (:pointer :double)))

(defcfun (replace-buffer "mpg123_replace_buffer") errors
  (handle :pointer)
  (data (:pointer :unsigned-char))
  (size size_t))

(defcfun (outblock "mpg123_outblock") size_t
  (handle :pointer))

(defcfun (replace-reader "mpg123_replace_reader") errors
  (handle :pointer)
  (r_read :pointer)
  (r_lseek :pointer))

(defcfun (replace-reader-handle "mpg123_replace_reader_handle") errors
  (handle :pointer)
  (r_read :pointer)
  (r_lseek :pointer)
  (cleanup :pointer))
