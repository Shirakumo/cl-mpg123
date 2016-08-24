#|
 This file is a part of cl-mpg123
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.mpg123)

;; low-level.lisp
(docs:define-docs
  (variable *here*
    "Variable containing the path to this very file, hopefully.")

  (variable *static*
    "Variable containing a pathname to the static directory.")
  )

;; conditions.lisp
(docs:define-docs
  (function define-simple-condition
    "Shorthand for DEFINE-CONDITION.
If the first argument in the body is a list, it is taken as the slots list.
Otherwise the arguments are the format string and arguments for the report.

In the report body, C is bound to the condition and S to the stream.")

  (function define-direct-condition
    "Expands to DEFINE-SIMPLE-CONDITION except automatically constructing slots for each access to C.

See DEFINE-SIMPLE-CONDITION")

  (type mpg-error
    "Condition superclass for all errors related to this library.")

  (type unknown-id3v2-frame-type
    "Condition signalled when an unknown id3v2 frame type name is requested from a metadata object.

See NAME
See METADATA
See MPG-ERROR")

  (function name
    "Returns the name of the erroneously requested id3v2 frame type.

See UNKNOWN-ID3v2-FRAME-TYPE")

  (type error-string-error
    "Condition superclass for all errors that include an error string from the underlying library.

See MPG-ERROR
See ERROR-STRING")

  (type generic-error
    "A generic error class to be used in situations where no more specific class applies.

See FORM
See ERROR-STRING-ERROR")

  (function form
    "Returns he form that caused the generic error.

See GENERIC-ERROR")

  (type init-failed
    "Condition signalled in the case that the initialisation of the library fails.

See INIT
See ERROR-STRING-ERROR")

  (type mpg-file-error
    "Condition superclass for all errors that relate to a FILE object.

See FILE
See MPG-ERROR")

  (function file
    "Returns the file associated with the condition.

See FILE
See MPG-FILE-ERROR")

  (type mpg-file-string-error
    "Combination of MPG-FILE-ERROR and ERROR-STRING-ERROR.

See MPG-FILE-ERROR
See ERROR-STRING-ERROR")

  (type handler-creation-failed
    "Condition signalled when creating a new mpg handler object fails.

See MPG-FILE-STRING-ERROR")

  (type not-connected
    "Condition signalled when an attempt is made to use a function that requires the file object to be connected, but it isn't.

See MPG-FILE-ERROR")

  (type connection-failed
    "Condition signalled when connecting to/opening the underlying file fails.

See CONNECT
See MPG-FILE-STRING-ERROR
See PATH")

  (type disconnection-failed
    "Condition signalled when disconnecting from/closing the underlying file fails.

See DISCONNECT
See MPG-FILE-STRING-ERROR")

  (type decoder-set-failed
    "Condition signalled when changing the decoder of a file fails.

See VALUE
See (SETF DECODER)
See MPG-FILE-STRING-ERROR")

  (function value
    "Returns the value the function tried to set before the condition was signalled.")

  (type read-failed
    "Condition signalled when mpg123 fails to read into a buffer.

See BUFFER
See BUFFER-SIZE
See MPG-FILE-STRING-ERROR
See DIRECT-READ")

  (function buffer
    "Returns the pointer to the C char buffer.")

  (function buffer-size
    "Returns the size of the buffer in bytes.")

  (function decode-failed
    "Condition signalled when mpg123 fails to decode a buffer into another.

See IN-BUFFER
See OUT-BUFFER
See IN-SIZE
See OUT-SIZE
See DECODE
See MPG-FILE-STRING-ERROR")

  (function in-buffer
    "Returns the pointer to the C char buffer used for input on decoding.")

  (function out-buffer
    "Returns the pointer to the C char buffer used for output on decoding.")

  (function in-size
    "Returns the size of the input buffer in bytes.")

  (function out-size
    "Returns the size of the output buffer in bytes.")

  (type frame-decode-failed
    "Condition signalled when mpg123 fails to decode a frame.

See MPG-FILE-STRING-ERROR
See DECODE-FRAME")

  (type query-failed
    "Generic condition signalled when the querying for a value failed.

See QUERY
See MPG-FILE-ERROR
See GENERIC-ERROR")

  (function query
    "The function that performed the query that failed.")

  (type seek-failed
    "Condition signalled when a seek to a different position in the file failed.

See BY
See MODE
See SEEK-POSITION
See MPG-FILE-ERROR
See SEEK")

  (function by
    "Returns the way by which the seek was performed.
Must be one of :SAMPLE :FRAME :SECOND

See SEEK-FAILED")

  (function mode
    "Returns the mode by which the seek was performed.
Must be one of :ABSOLUTE :RELATIVE :FROM-END

See SEEK-FAILED")

  (function seek-position
    "Returns the position to which the seek was performed.

See SEEK-FAILED")

  (type equalizer-query-failed
    "Condition signalled when querying for an equalizer band value failed.

See BAND
See CHANNEL
See MPG-FILE-ERROR
See EQUALIZER")

  (function band
    "Returns the band that was attempted to be accessed.")

  (function channel
    "Returns the channel that was attempted to be accessed.")

  (type equalizer-set-failed
    "Condition signalled when setting an equalizer band value failed.

See BAND
See CHANNEL
See MPG-FILE-STRING-ERROR
See (SETF EQUALIZER)")

  (type equalizer-reset-failed
    "Condition signalled when resetting the equalizer failed.

See MPG-FILE-STRING_ERROR
See RESET-EQUALIZER")

  (type volume-query-failed
    "Condition signalled when querying for the current volume failed.

See MPG-FILE-STRING-ERROR
See VOLUME")

  (type volume-set-failed
    "Condition signalled when setting the current volume failed.

See MPG-FILE-STRING-ERROR
See (SETF VOLUME)
See RELATIVE
See VALUE")

  (function relative
    "Returns whether the volume was attempted to be adjusted relatively or absolutely.")

  (type scan-failed
    "Condition signalled when scanning the file for metadata failed.

See MPG-FILE-STRING-ERROR
See SCAN")

  (type id3-query-failed
    "Condition signalled when retrieving the ID3 metadata failed.

See MPG-FILE-STRING-ERROR
See METADATA"))

;; id3-data.lisp
(docs:define-docs
  (variable *id3v1-genre-list*
    "A list of genres that the ID3V1 genre index references. Includes WinAmp extensions.
See https://de.wikipedia.org/wiki/Liste_der_ID3v1-Genres")

  (function id3v1-genre
    "Returns the name of the corresponding genre of the integer as defined by id3v1.")

  (function id3v2-genre
    "Parses the ID3V2 genre string into a list of genres.
More specifically, it handles ID3V1 references, of which there can be multiple.")

  (variable *id3v2-type-map*
    "A map of id3v2 frame type name to human-readable names.
See http://id3.org/d3v2.3.0")

  (function id3v2-type
    "Returns the id3v2 frame type keyword for the passed equivalent human-readable name."))

;; metadata.lisp
(docs:define-docs
  (type metadata
    "Class container for ID3 metadata.

When re/initialising this, you should pass the ID3V1/2 struct pointers as
the respective initargs. Note that ID3V1 and ID3V2 values will coexist, but
ID3V2 values should always come before ID3V1 values in the resulting fields
list. Also note that duplicate (by EQUAL) entries are not stored.

Note that when the metadata is reinitialised, all fields and pictures are
cleared out first, before potentially being repopulated by the passed args.

Note that ID3V1 text values are by default parsed by UTF-8 first and by
ISO-8859-1 if that fails. See DIRECT-STR. You can override the UTF-8 default
by specifying the :ID3V1-ENCODING initarg.

See METADATA
See VERSION
See FIELDS
See PICTURES
See FIELD
See FIELD-TEXT")

  (function version
    "The version of the ID3 metadata.
Can be either \"1.0\" \"1.1\" \"2.3\" or \"2.4\".

See METADATA")

  (function fields
    "Returns the list of fields stored in the metadata.
Each entry in the list is of the following form:

ENTRY       ::= (TYPE LANG DESCRIPTION TEXT)
TYPE        --- A keyword corresponding to the id3v2 frame type name.
LANG        --- A three-letter language string or NIL if no language was given.
DESCRIPTION --- A string describing the field or NIL if no description was given.
TEXT        --- The actual text content. Can also be an integer in cases where
                the data type was known (year, track number).

Note that fields that can potentially contain multiple values of the same type
will be split into multiple instances of the same field. For example, a TPE1
frame with text \"FOO/BAR\" gets split into two TPE1 entries, one with FOO as
text and one with BAR.

See METADATA
See FIELD
See FIELD-TEXT")

  (function pictures
    "Returns the list of pictures stored in the metadata.

See PICTURE
See METADATA")

  (function field
    "Gathers all fields matching the requested type into a list.

The field type is excluded, so each match is a list of three values:
language, description, and text.

On an unknown ID3V2 frame type name, an error is signalled.

See METADATA
See FIELDS
See ID3V2-TYPE")

  (function field-text
    "Returns all text values of the matching fields as multiple values.

This is mostly for convenience when you only care about one or more text
values of a field.

See FIELD")

  (type picture
    "Container class for a picture encapsulated in an ID3 tag.

The data is copied over to a lisp (unsigned-byte 8) vector, so the picture 
data will stay fresh even if the source file or id3 data is freed.

See KIND
See DESCRIPTION
See MIME-TYPE
See DATA")

  (function kind
    "Returns the picture type.

Can be one of:
:OTHER :ICON :OTHER-ICON :FRONT-COVER :BACK-COVER :LEAFLET :MEDIA :LEAD
:ARTIST :CONDUCTOR :ORCHESTRA :COMPOSER :LYRICIST :LOCATION :RECORDING
:PERFORMANCE :VIDEO :FISH :ILLUSTRATION :ARTIST-LOGO :PUBLISHER-LOGO

See PICTURE")

  (function description
    "Returns the picture description string.

See PICTURE")

  (function mime-type
    "Returns the picture data's mime-type as a string.

See PICTURE")

  (function data
    "Returns the actual picture data as an octet-vector.

You are responsible for converting or interpreting the data as needed.

See PICTURE."))

;; toolkit.lisp
(docs:define-docs
  (function with-foreign-values
    "Same as CFFI:WITH-FOREIGN-OBJECTS except resolving the bindings at the end and returning them as multiple values.")

  (function with-value-args
    "Same as CFFI:WITH-FOREIGN-OBJECTS except that after CALL the bindings contain the resolved values.")

  (function with-error
    "Handle a form that returns a CL-MPG123-CFFI:ERRORS enum value.")

  (function with-generic-error
    "Simple variant of WITH-ERROR that signals a generic variant.
Uses of this should be replaced by more explicit condition signals later.")

  (function with-negative-error
    "Handle a form that errors when its result is negative.")

  (function with-zero-error
    "Handle a form that errors when its result is zero.")

  (function string-nil
    "When the argument is NIL or an empty string, return NIL, otherwise return the argument.")

  (function direct-str
    "Translate the string pointed to by POINTER into a string of max LENGTH.
If the translated string is empty, NIL is returned.

At first the string is attempted to be decoded by the specified ENCODING. If this
fails, it is instead decoded by ISO-8859-1.")

  (function mstring
    "Translates an CL-MPG123-CFFI:MSTRING struct or pointer to such a struct to a string.
If the translated string is empty, NIL is returned.")

  (function split
    "Splits the string on each occurrence of char, dropping empty substrings."))

;; wrapper.lisp
(docs:define-docs
  (variable *print-object-path-limit*
    "Sets the amount of characters printed of the path of the FILE during PRINT-OBJECT.

See FILE")

  (variable *init*
    "Stores whether CL-MPG123-CFFI:INIT has been called or not.")

  (function init
    "Prepares the underlying library if not already done so.")

  (function exit
    "Cleans up the underlying library if not already done so.")

  (function encode-encodings
    "Encodes the list of encodings as a single integer by ORing their values together.")

  (function encode-channels
    "Encodes the list of channels as a single integer by ORing their values together.")

  (function decode-flags
    "Decodes the flags integer into a list of flag enum keywords.")

  (function dispose-handle
    "Cleans up the handle and deallocates it.")

  (function make-file
    "Create a new file object pointing to the given path.

See FILE")

  (type file
    "Container class for a file.

Manages all the decoding information so that it is easily accessible.
Also takes care of managing the underlying C values so that you don't
have to worry about garbage collection or buffer allocation and so forth.
It also takes care of setting up the desired decoding parameters and flags.

The slots of the file are not settable, however you can reconfigure the
file at any point using REINITIALIZE-INSTANCE. Note that this will have
to DISCONNECT the file and delete its handle first. It will reCONNECT
if it was previously connected, but you cannot do this if you are in the
middle of processing data or need to otherwise preserve state.

You can find more information about most of the properties that can be set
via initargs in the mpg123.h mpg123_params and mpg123_param_flags enums.
See https://www.mpg123.de/api/group__mpg123__init.shtml#ga73a8ff3363028b89afc72b3ea032b9cb

See HANDLE
See CONNECTED
See SCANNED
See BUFFER
See RATE
See CHANNELS
See ENCODING
See PATH
See DECODER
See ACCEPTED-FORMAT
See BUFFER-SIZE
See FORCE-RATE
See DOWN-SAMPLE
See RVA
See DOWNSPEED
See UPSPEED
See START-FRAME
See DECODE-FRAMES
See OUTSCALE
See INDEX-SIZE
See PREFRAMES
See FORCE-CHANNELS
See FORCE-8BIT
See GAPLESS
See FUZZY-SEEK
See FORCE-FLOAT
See SKIP-ID3V2
See IGNORE-INFOFRAME
See AUTO-RESAMPLE
See PICTURES
See CONNECT
See DISCONNECT
See READ-DIRECTLY
See PROCESS
See PROCESS-TO-VECTOR
See PROCESS-INTO-VECTOR
See DECODE
See DECODE-FRAME
See SAMPLE-POSITION
See FRAME-POSITION
See STREAM-POSITION
See SEEK
See TIME-FRAME-INDEX
See EQUALIZER
See RESET-EQUALIZER
See VOLUME
See INFO
See FILE-FORMAT
See SCAN
See FRAME-COUNT
See SAMPLE-COUNT
See FRAME-SECONDS
See FRAME-SAMPLES
See TRACK-LENGTH
See METADATA")

  (function handle
    "Returns the CL-MPG123-CFFI:HANDLE pointer of the file.

See FILE")

  (function connected
    "Returns whether the file has been connected to the file or not.

Before being connected most queries or actions against the file will
fail.

See FILE
See CONNECT")

  (function scanned
    "Returns whether the file has been scanned for metadata.

Before being scanned, some queries will return inaccurate results
or might not be able to return any data at all, in which case they will
initiate a scan themselves automatically.

See FILE
See SCAN")

  (function buffer
    "Returns a pointer to the C char buffer that is used to store decoded data.

This might be NIL if the buffer-size is NIL.

See FILE
See BUFFER-SIZE")

  (function rate
    "Returns the rate in Hertz that the decoded file is in.

This is NIL until the file is connected.

See FILE
See CONNECT
See FILE-FORMAT")

  (function channels
    "Returns the number of channels the decoded file has.

This is NIL until the file is connected.

See FILE
See CONNECT
See FILE-FORMAT")

  (function encoding
    "Returns the encoding the file is in.

This is NIL until the file is connected.

See FILE
See CONNECT
See FILE-FORMAT")

  (function path
    "Returns the pathname of the file being accessed.

See FILE
See CONNECT")

  (function decoder
    "The decoder driver to use as backend.

See SUPPORTED-DECODERS for a list of decoders that can be used as a backend.

This is SETFable.

See FILE
See DECODER-SET-FAILED")

  (function accepted-format
    "Returns the accepted format for the decoder.

This can be NIL for none, T for all, or a list of three values:
 (RATE CHANNELS ENCODINGS) where RATE is the accepted rate in Hertz,
CHANNELS is one of :left :right :left-right, and ENCODINGS is a list of
allowed data encodings, each value being one of:
:int8  :uint8  :int16  :uint16 :int24 :uint24
:int32 :uint32 :ulaw-8 :alaw-8 :float :double

This can be set via the :accepted-format initarg.

See FILE
See CL-MPG123-CFFI:ENC")

  (function buffer-size
    "Returns the size of the internal buffer used to store decoded data.

This can be NIL for no buffer, T for automatic size, or an integer specifying
the number of bytes to use. The automatic size is picked by 
CL-MPG123-CFFI:OUTBLOCK.

This can be set via the :buffer-size initarg.

See FILE
See BUFFER")

  (function force-rate
    "Returns, if forced, the rate at which output is produced.

This can be set via the :force-rate initarg.

See FILE")

  (function down-sample
    "Returns the downsampling approach being used.

Can be one of NIL :NATIVE :HALF-RATE :QUARTER-RATE.

This can be set via the :down-sample initarg.

See FILE")

  (function rva
    "Returns the RVA mode being used.

Can be one of NIL :OFF :MIX :ALBUM.

This can be set via the :down-sample initarg.

See FILE")

  (function downspeed
    "Returns the number of times a frame will be played.

This can be set via the :downspeed initarg.

See FILE")

  (function upspeed
    "Returns the number of frames between two played frames.

This can be set via the :upspeed initarg.

See FILE")

  (function start-frame
    "Returns the index of the starting frame. Frames before this are not played.

This can be set via the :start-frame initarg.

See FILE")

  (function decode-frames
    "Returns the number of frames that are decoded or T for all.

This can be set via the :decode-frames initarg.

See FILE")

  (function outscale
    "Returns the output sample amplitude scale.

This can be set via the :outscale initarg.

See FILE")

  (function index-size
    "Returns the frame index size.

Can be NIL for default, T for dynamic growth, or an integer for
specific size (positive) or growth rate (negative).

This can be set via the :index-size initarg.

See FILE")

  (function preframes
    "Returns how many frames to decode or ignore in advance for layer 3.

This can be set via the :preframes initarg.

See FILE")

  (function force-channels
    "Returns, if at all, which channels are forced.

Can be one of NIL :mono-right :mono-left :mono-mix :stereo.

This can be set via the :force-channels initarg.

See FILE")

  (function force-8bit
    "Returns whether to force 8bit formats.

This can be set via the :force-8bit initarg.

See FILE")

  (function gapless
    "Returns whether to use gapless decoding.

This can be set via the :gapless initarg.

See FILE")

  (function fuzzy-seek
    "Returns whether to allow approximate seeking by guessing.

This can be set via the :fuzzy-seek initarg.

See FILE")

  (function force-float
    "Returns whether to force floating-point output.

This can be set via the :force-float initarg.

See FILE")

  (function skip-id3v2
    "Returns whether to skip the ID3V2 tag information.

This can be set via the :skip-id3v2 initarg.

See FILE")

  (function ignore-infoframe
    "Returns whether to ignore LAME/Xing info frames and treat them as normal MPEG data.

This can be set via the :ignore-infoframe initarg.

See FILE")

  (function auto-resample
    "Returns whether to automatic internal resampling of any kind.

This can be set via the :auto-resample initarg.

See FILE")

  (function parse-pictures
    "Returns whether ID3V2 APIC tags should be parsed.

This can be set via the :parse-pictures initarg.

See FILE")

  (function check-connected
    "Checks whether the file is connected or not. If not, an error is signalled.

See FILE
See CONNECTED
See NOT-CONNECTED")

  (function connect
    "Connects the file to the underlying file on the filesystem.

This will trigger parsing of several pieces of information within
the file and open the path to querying the file for properties.
Naturally it will also allow actually decoding the file.

See FILE
See CONNECTED
See CONNECTION-FAILED")

  (function disconnect
    "Disconnects the file from the underlying file on the filesystem.

This will free up the file descriptor. And prevent you from decoding
any more data until the file is connected again.

See FILE
See CONNECTED
See DISCONNECTION-FAILED")

  (function read-directly
    "Decodes data from the file and stores it in BUFFER, reading at most BUFFER-SIZE bytes.

Returns the number of bytes that were stored in the buffer.

See FILE
See PROCESS
See READ-FAILED")

  (function process
    "Decodes data from the file into the internal buffer.

Note that this will fail grossly if the file's buffer-size / buffer is NIL.

Returns the number of bytes read. See BUFFER for the pointer to the C buffer
where the actual data is stored.

See READ-DIRECTLY
See BUFFER
See BUFFER-SIZE")

  (function process-to-vector
    "Decodes data from the file into a new (unsigned-byte 8) vector.

The returned vector will have the exact length fitting to the amount of
data decoded.

See PROCESS")

  (function process-into-vector
    "Decodes data from the file into the given octet-vector.

Returns the number of bytes read.

At most (min (length vector) (buffer-size file)) bytes are read.

See PROCESS")

  (function decode-directly
    "Decodes data from the given input array pointer to the given output array pointer.

Returns the number of bytes decoded.

See FILE
See DECODE-FAILED")

  (function decode
    "Decodes data from the given input vector to the given output vector.

Returns the number of bytes decoded.

See DECODE-DIRECTLY")

  (function decode-frame
    "Decodes the next frame into the library's internal buffer.

This does not store it into the file buffer! BUFFER will not have
changed!

See FILE
See FRAME-DECODE-FAILED")

  (function sample-position
    "Returns the current position as a sample index.

See FILE")

  (function frame-position
    "Returns the current position as a frame index.

See FILE")

  (function stream-position
    "Returns the current position in stream as a byte index.

See FILE")

  (function seek
    "Seek in the frame.

MODE can be one of :ABSOLUTE :RELATIVE :FROM-END which decides
whether the given position should be absolute, relative to the
current position, or from the end, respectively.

BY can be one of :SAMPLE :FRAME :SECOND which decides whether
seeking should be by the sample, frame, or track seconds index.

Returns the new position as sample, frame, or track seconds index
respectively.

See FILE
See SEEK-FAILED")

  (function time-frame-index
    "Returns the frame index of a given track seconds index.

See FILE")

  (function equalizer
    "Accesses the equalizer value for the given channel and band.

Channel must be within [0,31].

This is SETFable.

See EQUALIZER-QUERY-FAILED
See EQUALIZER-SET-FAILED
See FILE")

  (function reset-equalizer
    "Resets the equalizer to its initial values. Which is to say it disables it.

See EQUALIZER-RESET-FAILED
See FILE")

  (function volume
    "Accesses the volume of the file.

Returned are three values: the base volume, the actual volume including
potential changes by RVA, and the RVA adjustment made. The volumes are
linear factors, and the RVA adjustment is in decibel.

This is SETFable. On SETF, an additional keyword argument :relative
is supported, which says whether to adjust volume relatively to the
current value, or absolutely.

See VOLUME-QUERY-FAILED
See VOLUME-SET-FAILED
See FILE")

  (function info
    "Returns various information about the current frame.

Returned is a plist with the following fields:
:VERSION   The MPEG version, 1.0/2.0/2.5
:LAYER     The MPEG layer, 1/2/3
:RATE      The sample rate in Hertz
:MODE      The channel mode
:MODE-EXT  The mode extension bit flag
:FLAGS     A list of frame flags
:EMPHASIS  The emphasis type
:BITRATE   The bitrate in kbps 
:ABR-RATE  The target average bitrate
:VBR       The VBR mode

See FILE")

  (function file-format
    "Returns information about the file's actual format.

Returned are three values, the rate in Hertz, the channels, and
the encoding used.

See RATE
See CHANNELS
See ENCODING
See FILE")

  (function scan
    "Scans the file for information.

This is only done if a scan has not already been performed.

See SCANNED
See FILE
See SCAN-FAILED")

  (function frame-count
    "Returns the number of frames in the file.

Performs a SCAN if necessary.

See FILE
See SCAN")

  (function sample-count
    "Returns the number of samples in the file.

Performs a SCAN if necessary.

See FILE
See SCAN")

  (function frame-seconds
    "Returns the number of seconds a frame encompasses.

Performs a SCAN if necessary.

See FILE
See SCAN")

  (function frame-samples
    "Returns the number of samples per frame.

Performs a SCAN if necessary.

See FILE
See SCAN")

  (function track-length
    "Returns the length of the track in seconds.

See FRAME-SECONDS
See FRAME-COUNT")

  (function track-position
    "Returns the current track seconds position.

See FRAME-SECONDS
See FRAME-POSITION")

  (function metadata
    "Returns a fresh metadata object created from the scanned ID3 tags.

Performs a SCAN if necessary.

The :ID3V1-ENCODING argument specifies which encoding to use for the
strings in ID3V1 tags, as that is not formally specified and might
vary by file.

See FILE
See SCAN
See METADATA")

  (function format-time
    "Formats the given seconds in H:MM:SS format.")

  (function decoders
    "Returns a list of all decoder backends the library knows about.

See SUPPORTED-DECODERS")

  (function supported-decoders
    "Returns a list of all supported decoder backends.

See DECODERS
See DECODER")

  (function supported-encodings
    "Returns a list of all supported encoding types."))
