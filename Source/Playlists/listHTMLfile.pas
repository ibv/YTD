unit listHTMLfile;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  PCRE, HttpSend,
  uDownloader, uCommonDownloader, uPlaylistDownloader, listHTML,
  uDownloadClassifier;

type
  TPlaylist_HTMLfile = class(TPlaylist_HTML)
    private
    protected
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod = hmGET): boolean; override;
    public
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uFiles;
  
{ TPlaylist_HTMLfile }

constructor TPlaylist_HTMLfile.Create(const AMovieID: string);
begin
  inherited;
end;

destructor TPlaylist_HTMLfile.Destroy;
begin
  inherited;
end;

function TPlaylist_HTMLfile.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod): boolean;
var MS: TMemoryStream;
    s: AnsiString;
begin
  Result := FileExists(Url);
  if Result then
    begin
    MS := LoadFileIntoMemory(Url);
    try
      if MS.Size = 0 then
        Page := ''
      else
        begin
        SetLength(s, MS.Size);
        Move(MS.Memory^, s[1], MS.Size);
        Page := s;
        end;
    finally
      MS.Free;
      end;
    end;
end;

end.
