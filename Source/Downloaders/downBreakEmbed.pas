unit downBreakEmbed;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downBreak;

type
  TDownloader_BreakEmbed = class(THttpDownloader)
    private
    protected
      MovieVarsRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod): boolean; override;
      function AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean; override;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://media1.break.com/static/app/v1/global/swf/player10.swf?sVidLoc=http%3a%2f%2fvideo1.break.com%2fdnet%2fmedia%2f2007%2f8%2fdude-blows-off-firecracker-in-teeth.flv&sThumbLoc=http%3a%2f%2fmedia1.break.com%2fdnet%2fmedia%2f2007%2f8%2fdude-blows-off-firecracker-in-teeth.jpg&contentURL=http%3a%2f%2fwww.break.com%2findex%2fdude-blows-off-firecracker-in-teeth.html&sShareURL=http%3a%2f%2fwww.break.com%2findex%2fdude-blows-off-firecracker-in-teeth.html%23TellAFriendhttp%3a%2f%2fstats.break.com%2finvoke.txt&iContentID=359418&autoplay=0&embed=2&contentidencoded=359418&categoryid=4&userid=1620903&mode=embed&linktitle=Funny+Videos&sVidTitle=EMBED-Dude+Blows+Off+Firecracker+In+Teeth&icon=1B608EE7AFCE3765E176F3C6FBB98002B3D18C64572F2307D76FAB7CA970F3B1D7D7
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*break\.com/.*?';
  URLREGEXP_ID =        '\?(?:.*?&)*(?:sVidLoc|sVidTitle|icon)=.+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_EXTRACT_VARS = '[?&](?P<VARNAME>[^=]+)=(?P<VARVALUE>[^&]*)';

{ TDownloader_BreakEmbed }

class function TDownloader_BreakEmbed.Provider: string;
begin
  Result := TDownloader_Break.Provider;
end;

class function TDownloader_BreakEmbed.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

constructor TDownloader_BreakEmbed.Create(const AMovieID: string);
begin
  inherited;
  SetInfoPageEncoding(peNone);
  MovieVarsRegExp := RegExCreate(REGEXP_EXTRACT_VARS, [rcoIgnoreCase]);
end;

destructor TDownloader_BreakEmbed.Destroy;
begin
  RegExFreeAndNil(MovieVarsRegExp);
  inherited;
end;

function TDownloader_BreakEmbed.GetMovieInfoUrl: string;
begin
  Result := 'http://www.break.com'; // No download is needed, but this function must return something
end;

function TDownloader_BreakEmbed.GetMovieInfoContent(Http: THttpSend; Url: string; out Page: string; Method: THttpMethod): boolean;
begin
  Page := MovieID;
  Result := MovieID <> '';
end;

function TDownloader_BreakEmbed.AfterPrepareFromPage(var Page: string; Http: THttpSend): boolean;
const TitlePrefix = 'EMBED-';
var Url, Title, Token: string;
begin
  inherited AfterPrepareFromPage(Page, Http);
  Result := False;
  if not GetRegExpVarPairs(MovieVarsRegExp, Page, ['sVidLoc', 'sVidTitle', 'icon'], [@Url, @Title, @Token]) then
    SetLastErrorMsg(_(ERR_FAILED_TO_LOCATE_MEDIA_INFO_PAGE))
  else if Url = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['sVidLoc']))
  else if Title = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['sVidTitle']))
  else if Token = '' then
    SetLastErrorMsg(Format(_(ERR_VARIABLE_NOT_FOUND), ['icon']))
  else
    begin
    MovieUrl := UrlDecode(Url) + '?' + Token;
    if AnsiCompareText(TitlePrefix, Copy(Title, 1, Length(TitlePrefix))) = 0 then
      System.Delete(Title, 1, Length(TitlePrefix)); 
    SetName(UrlDecode(Title));
    SetPrepared(True);
    Result := True;
    end;
end;

initialization
  RegisterDownloader(TDownloader_BreakEmbed);

end.
