unit downLiveLeakEmbedded;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, HttpSend,
  uDownloader, uCommonDownloader, uHttpDownloader, downLiveLeak;

type
  TDownloader_LiveLeakEmbedded = class(TDownloader_LiveLeak)
    private
    protected
    public
      class function UrlRegExp: string; override;
    end;

implementation

uses
  uDownloadClassifier,
  uMessages;

// http://www.liveleak.com/e/57d_1278546690
const
  URLREGEXP_BEFORE_ID = '^https?://(?:[a-z0-9-]+\.)*liveleak\.com/e/';
  URLREGEXP_ID =        '[^/?&]+';
  URLREGEXP_AFTER_ID =  '';

{ TDownloader_LiveLeak }

class function TDownloader_LiveLeakEmbedded.UrlRegExp: string;
begin
  Result := URLREGEXP_BEFORE_ID + '(?P<' + MovieIDParamName + '>' + URLREGEXP_ID + ')' + URLREGEXP_AFTER_ID;
end;

initialization
  RegisterDownloader(TDownloader_LiveLeakEmbedded);

end.

