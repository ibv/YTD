unit downCT24MSFotbal_V2;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend, SynaCode,
  uDownloader, uCommonDownloader, uMSDownloader, downCT24MSFotbal;

type
  TDownloader_CT24MSFotbal_V2 = class(TDownloader_CT24MSFotbal)
    private
    protected
      function GetMovieInfoUrl: string; override;
    public
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uXML,
  uDownloadClassifier,
  uMessages;

// http://msfotbal.ct24.cz/video.asp?video_id=95
// http://msfotbal.ct24.cz/video.asp
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*msfotbal\.ct24\.cz/';
  URLREGEXP_ID =        'video\.asp(?:\?video_id=[0-9]+)?';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = '<h2>(?P<TITLE>.*?)</h2>';

const
  SOAP_URL = 'http://ctdir.visual.cz/ivysilani/services/streaming/SLP.asmx';
  SOAP_ACTION = 'http://ivysilani.visual.cz/services/GetPlaylistUrl';
  SOAP_REQUEST = '<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">' +
                   '<s:Body>' +
                     '<GetPlaylistUrl xmlns:i="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://ivysilani.visual.cz/services">' +
                       '<request>' +
                         '<Format>%s</Format>' +
                         '<ClientAddress>%s</ClientAddress>' +
                         '<Expiration>%s</Expiration>' +
                         '<Playlist>' +
                           '<PlaylistItem>' +
                             '<Type>Archive</Type>' +
                             '<Identifier>%s</Identifier>' +
                             '<Begin>0</Begin>' +
                             '<Duration i:nil="true" /> ' +
                             '<NoSkip i:nil="true" /> ' +
                           '</PlaylistItem>' +
                         '</Playlist>' +
                       '</request>' +
                     '</GetPlaylistUrl>' +
                   '</s:Body>' +
                 '</s:Envelope>';

{ TDownloader_CT24MSFotbal_V2 }

class function TDownloader_CT24MSFotbal_V2.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_CT24MSFotbal_V2.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peUTF8);
  RegExFreeAndNil(MovieTitleRegExp);
  MovieTitleRegExp := RegExCreate(REGEXP_MOVIE_TITLE, [rcoIgnoreCase, rcoSingleLine]);
end;

destructor TDownloader_CT24MSFotbal_V2.Destroy;
begin
  RegExFreeAndNil(MovieTitleRegExp);
  inherited;
end;

function TDownloader_CT24MSFotbal_V2.GetMovieInfoUrl: string;
begin
  Result := 'http://msfotbal.ct24.cz/' + MovieID;
end;

initialization
  RegisterDownloader(TDownloader_CT24MSFotbal_V2);

end.
