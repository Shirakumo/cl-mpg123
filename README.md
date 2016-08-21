## About cl-mpg123
This is a bindings and wrapper library to [libmpg123](https://www.mpg123.de) allowing for convenient, extensive, and fast decoding of MPEG1/2/3 (most prominently mp3) files.

## How To
Precompiled versions of the underlying library are included in this. If you want to build it manually however, refer to the [mpg123](https://www.mpg123.de/) page.

Load the system through ASDF or Quicklisp:

    (ql:quickload :cl-mpg123)

Create a new `file` object:

    (defvar *file* (cl-mpg123:make-file #p"~/my-cool-music.mp3"))

This will initialise a file and set the requested parameters. As it is now, we've left everything up to defaults. If you need more tailored control as to how the file will be treated, see the `file` class documentation. Next we will want to actually open up the underlying file so that we can look into it.

    (cl-mpg123:connect *file*)

Processing the actual sound data, is just a matter of calling `process` and passing the `buffer` to something that can do something useful with the raw audio data. However, unless you specifically chose the audio data format yourself, you'll first want to query that:

    (cl-mpg123:file-format *file*)

This will give you the rate, channels, and encoding of the file so that you can configure your audio processing library accordingly. Next we can process the data in a loop:

    (loop with buffer = (cl-mpg123:buffer *file*)
          for bytes = (cl-mpg123:process *file*)
          until (= 0 bytes)
          do (process-buffer-somehow buffer bytes))

This library is also capable of extracting ID3 tags from the file. A quick summary of information found in a file can be displayed with `describe`:

    (describe *file*)

Direct access to the metadata object is possible with `metadata`. This object will then allow you to retrieve the values of various tags through `field` and `field-text`:

    (cl-mpg123:field-text :album (metadata *file*))

For the list of possible field names, see `*id3v2-type-map`. For a listing of all recognised tags, simply use `fields` on the `metadata` object. If possible, picture tags are also extracted and converted. You can get your hands on those through the `pictures` reader. Returned are a list of `picture` objects that will encapsulate the data.

Should you wish to change parameters for the decoding or the file itself, you can do so via `reinitialize-instance`. Just be aware that doing so will close the handle and is thus not recommended during processing.

Also included in `libmpg123` are volume changing and a full 32 band equalizer. The volume can be accessed by, naturally, `volume`, and the equalizer by `equalizer`.

    (setf (cl-mpg123:volume *file*) 0.5)
    (cl-mpg123:volume *file*)

You can also seek around in the file if necessary. Doing so is done by the single `seek` function which allows you to seek by samples, frames, or track seconds, either relatively, absolutely, or from the end of the file.

    (cl-mpg123:seek *file* 1 :by :second)

Once you know you are done with the file, you can `disconnect` it to release the underlying file descriptor. Once the file is GCed, its handler and buffer will automatically be freed so that there's no worry about permanent memory leaks.

    (cl-mpg123:disconnect *file*)

And that's about it. See the individual functions for more information on what exactly you can get or change. If you want to look at a full example that actually plays something, see the `cl-mpg123-example` system, which shows how to use it in combination with `cl-out123` using either the low-level binding interface, or the high-level wrapper interface shown here.

## Also See

* [cl-out123](https://shirakumo.github.io/cl-out123/) For simple audio output.
