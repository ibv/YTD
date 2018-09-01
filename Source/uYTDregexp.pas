unit uYTDregexp;

interface

const
  SUPPORTED_URLS_REGEXP = '^'
    // Youtube:
    //     http://www.youtube.com/v/HANqEpKDHyk
    //     http://www.youtube.com/watch/v/HANqEpKDHyk
    //     http://www.youtube.com/watch?v=eYSbVcjyVyw
    + '(?:https?://(?:www\.)?youtube\.com/(?:v/|watch/v/|watch\?v=)(?P<YOUTUBE>[^&?]+))|'
    // N-Joy:
    //     http://n-joy.cz/video/supcom-2-zabery-z-hrani-2/oiuhz6e3xgt35e4e
    + '(?:^https?://(?:www\.)?n-joy\.cz/video/[^/]+/(?P<NJOY>[^/&?]+))|'
    // Blip.tv:
    //     http://blip.tv/play/hIVV4sNUAg
    //     http://blip.tv/file/108391
    + '(?:^https?://(?:[a-z0-9-]+\.)?blip\.tv/play/(?P<BLIPTV>[^/&?]+))|'
    + '(?:^https?://(?:[a-z0-9-]+\.)?blip\.tv/file/(?P<BLIPTVV2>[0-9]+))|'
    // Stream.cz:
    //     http://www.stream.cz/reklamozrouti/410282-reklamozrouti-medvedi-reklama
    //     http://www.stream.cz/video/410282-reklamozrouti-medvedi-reklama
    //     http://www.stream.cz/object/410282-reklamozrouti-medvedi-reklama
    + '(?:^https?://(?:[a-z0-9-]+\.)?stream\.cz/(?:[^/]+/)*(?P<STREAM>[0-9]+))|'
    // iPrima.cz:
    //     http://www.iprima.cz/videoarchiv/44524/all/all
    + '(?:^https?://(?:[a-z0-9-]+\.)?iprima.cz/videoarchiv/(?P<IPRIMA>[0-9]+))|'
    ;

implementation

end.
