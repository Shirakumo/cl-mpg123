(in-package #:org.shirakumo.fraf.mpg123)

(defparameter *id3v1-genre-list*
  (list ;; ID3V1 Spec
        "Blues"
        "Classic Rock"
        "Country"
        "dance"
        "Disco"
        "Funk"
        "Grunge"
        "Hip-Hop"
        "Jazz"
        "Metal"
        "New Age"
        "Oldies"
        "Other"
        "Pop"
        "Rhythm and Blues"
        "Rap"
        "Reggae"
        "Rock"
        "Techno"
        "Industrial"
        "Alternative"
        "Ska"
        "Death Metal"
        "Pranks"
        "Soundtrack"
        "Euro-Techno"
        "Ambient"
        "Trip-Hop"
        "Vocal"
        "Jazz & Funk"
        "Fusion"
        "Trance"
        "Classical"
        "Instrumental"
        "Acid"
        "House"
        "Game"
        "Sound Clip"
        "Gospel"
        "Noise"
        "Alternative Rock"
        "Bass"
        "Soul"
        "Punk"
        "Space"
        "Meditative"
        "Instrumental Pop"
        "Instrumental Rock"
        "Ethnic"
        "Gothic"
        "Darkwave"
        "Techno-Industrial"
        "Electronic"
        "Pop-Folk"
        "Eurodance"
        "Dream"
        "Southern Rock"
        "Comedy"
        "Cult"
        "Gangsta"
        "Top 40"
        "Christian Rap"
        "Pop / Funk"
        "Jungle"
        "Native US"
        "Cabaret"
        "New Wave"
        "Psychedelic"
        "Rave"
        "Showtunes"
        "Trailer"
        "Lo-Fi"
        "Trival"
        "Acid Punk"
        "Acid Jazz"
        "Polka"
        "Retro"
        "Musical"
        "Rock 'n' Roll"
        "Hard Rock"
        ;; Winamp Extensions
        "Folk"
        "Folk-Rock"
        "National Folk"
        "Swing"
        "Fast Fusion"
        "Bebop"
        "Latin"
        "Revival"
        "Celtic"
        "Bluegrass"
        "Avantgarde"
        "Gothic Rock"
        "Progressive Rock"
        "Psychedelic Rock"
        "Symphonic Rock"
        "Slow Rock"
        "Big Band"
        "Chorus"
        "Easy Listening"
        "Acoustic"
        "Humour"
        "Speech"
        "Chanson"
        "Opera"
        "Chamber Music"
        "Sonata"
        "Symphony"
        "Booty Bass"
        "Primus"
        "Porn Groove"
        "Satire"
        "Slow Jam"
        "Club"
        "Tango"
        "Samba"
        "Folklore"
        "Ballad"
        "Power Ballad"
        "Rhythmic Soul"
        "Freestyle"
        "Duet"
        "Punk Rock"
        "Drum Solo"
        "A capella"
        "Euro-House"
        "Dance Hall"
        "Goa"
        "Drum & Bass"
        "Club-House"
        "Hardcore Techno"
        "Terror"
        "Indie"
        "BritPop"
        "Negerpunk"
        "Polsk Punk"
        "Beat"
        "Christian Gangsta Rap"
        "Heavy Metal"
        "Black Metal"
        "Crossover"
        "Contemporary Christian"
        "Christian Rock"
        "Merengue"
        "Salsa"
        "Trash Metal"
        "Anime"
        "Jpop"
        "Synthpop"
        "Abstract"
        "Art Rock"
        "Baroque"
        "Bhangra"
        "Big Beat"
        "Breakbeat"
        "Chillout"
        "Downtempo"
        "Dub"
        "EBM"
        "Electric"
        "Electo"
        "Electroclash"
        "Emo"
        "Experimental"
        "Garage"
        "Global"
        "IDM"
        "Illbient"
        "Industro-Goth"
        "Jam Band"
        "Krautrock"
        "Leftfield"
        "Lounge"
        "Math Rock"
        "New Romantic"
        "Nu-Breakz"
        "Post-Punk"
        "Post-Rock"
        "Psytrance"
        "Shoegaze"
        "Space Rock"
        "Trop Rock"
        "World Music"
        "Neoclassical"
        "Audiobook"
        "Audio Theatre"
        "Neue Deutsche Welle"
        "Podcast"
        "Indie Rock"
        "G-Funk"
        "Dubstep"
        "Garage Rock"
        "Psybient"))

(defun id3v1-genre (num)
  (nth num *id3v1-genre-list*))

(defun id3v2-genre (genre)
  (let ((pos 0)
        (genres ()))
    (tagbody
     dispatch
       (cond ((<= (length genre) pos)
              (go end))
             ((char= (char genre pos) #\()
              (go start-bracket))
             (T
              (go read-rest)))
     start-bracket
       (incf pos)
       (if (char= (char genre pos) #\()
           (go read-rest)
           (go read-num))
     read-num
       (let ((start pos))
         (loop until (char= (char genre pos) #\)) do (incf pos))
         (pushnew (id3v1-genre (parse-integer genre :start start :end pos)) genres :test #'string-equal)
         (incf pos)
         (go dispatch))
     read-rest
       (pushnew (subseq genre pos) genres :test #'string-equal)
       (setf pos (length genre))
       (go dispatch)
     end
       (return-from id3v2-genre genres))))

(defparameter *id3v2-type-map*
  '((:aenc :encryption :audio-encryption)
    (:apic :picture)
    (:comm :comment)
    (:comr :commercial :commercial-frame)
    (:encr :encryption-method)
    (:equa :equalization)
    (:etco :timing-code :event-timing-codes)
    (:geob :object :general-encapsulated-object)
    (:grid :group :group-identification-registration)
    (:ipls :people :involved-people :involved-people-list)
    (:link :link :linked-information)
    (:mcdi :cd-identifier)
    (:mllt :mpeg-lookup-table)
    (:owne :ownership :ownership-frame)
    (:priv :private :private-frame)
    (:pcnt :play-counter)
    (:popm :popularimeter)
    (:poss :synchronisation-frame)
    (:rbuf :buffer :recommended-buffer-size)
    (:rvad :rva :relative-volume-adjustment)
    (:rvrb :rvb :reverb)
    (:sylt :synchronized-text)
    (:sytc :synchronized-tempo)
    (:talb :album :movie :show)
    (:tbpm :bpm :beats-per-minute)
    (:tcom :composer)
    (:tcon :genre :content-type)
    (:tcop :copyright :copyright-message)
    (:tdat :date)
    (:tdly :delay :playlist-delay)
    (:tenc :encoder :encoded-by)
    (:text :writer :lyricist :text-writer)
    (:tflt :file-type)
    (:time :time)
    (:tit1 :type :content-group :content-group-description)
    (:tit2 :title :songname :content-description)
    (:tit3 :subtitle :description-refinement)
    (:tkey :key :initial-key)
    (:tlan :language)
    (:tlen :length)
    (:tmed :media-type)
    (:toal :original-album :original-movie :original-show)
    (:tofn :original-filename)
    (:toly :original-writer :original-lyricist :original-text-writer)
    (:tope :original-artist :original-performer)
    (:tory :original-year :original-release-year)
    (:town :licensee :owner :file-licensee :file-owner)
    (:tpe1 :artist :lead-performer :soloist)
    (:tpe2 :album-artist :band :orchestra :accompaniment)
    (:tpe3 :conductor :performer-refinement)
    (:tpe4 :interpreter :remixer)
    (:tpos :part :part-of-set)
    (:tpub :publisher)
    (:trck :track :track-number :position-in-set)
    (:trda :recording-date)
    (:trsn :radio-name :internet-radio-station-name)
    (:trso :radio-owner :internet-radio-stsation-owner)
    (:tsiz :size)
    (:tsrc :isrc :international-standards-recording-code)
    (:tsse :recording-settings :software :hardware)
    (:tyer :year)
    (:txxx :extra :user-defined-information)
    (:ufid :unique-file-id :unique-file-identifier :id)
    (:user :tos :terms-of-use)
    (:uslt :synchronised-transcription :synchronised-lyric-transcription :synchronised-text-transcription)
    (:wcom :commercial-info)
    (:wcop :copyright-info :legal-info)
    (:woaf :track-webpage :official-audio-file-webpage)
    (:woar :artist-webpage :official-artist-webpage :official-performer-webpage)
    (:woas :source-webpage :official-audio-source-webpage)
    (:wors :radio-webpage :official-internet-radio-station-webpage)
    (:wpay :payment)
    (:wpub :publisher-webpage :official-publisher-webpage)
    (:wxxx :webpage :user-defined-link)))

(defun id3v2-type (name)
  (first (find name *id3v2-type-map* :test (lambda (name keys) (find name keys)))))
