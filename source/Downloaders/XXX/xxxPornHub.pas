(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2009-12 Pepak
http://www.pepak.net/ytd                                  http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2009-12 Pepak (http://www.pepak.net)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Pepak nor the
      names of his contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL PEPAK BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

******************************************************************************)

unit xxxPornHub;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXml, HttpSend, SynaCode,
  uCrypto, uStrings, uFunctions, 
  uDownloader, uCommonDownloader, uHttpDownloader;

type
  TDownloader_PornHub = class(THttpDownloader)
    private
    protected
      FlashVarsRegExp: TRegExp;
      FlashVarsItemsRegExp: TRegExp;
      FlashVarsItemsQualityRegExp: TRegExp;
    protected
      function GetMovieInfoUrl: string; override;
      function AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean; override;
      function Decrypt(const Data, Password: AnsiString): AnsiString;
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      constructor Create(const AMovieID: string); override;
      destructor Destroy; override;
    end;

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

const
  URLREGEXP_BEFORE_ID = 'pornhub\.com/view_video\.php\?(?:.*&)?viewkey=';
  URLREGEXP_ID =        '[0-9a-f]+';
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_FLASHVARS = REGEXP_FLASHVARS_JS;
  REGEXP_FLASHVARS_ITEMS = REGEXP_PARSER_FLASHVARS_JS;
  REGEXP_FLASHVARS_ITEMS_QUALITY = '"quality_(?P<QUALITY>\d+)[^"]*"\s*:\s*"(?P<URL>.+?)"';

{ TDownloader_PornHub }

class function TDownloader_PornHub.Provider: string;
begin
  Result := 'PornHub.com';
end;

class function TDownloader_PornHub.UrlRegExp: string;
begin
  Result := Format(REGEXP_COMMON_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

constructor TDownloader_PornHub.Create(const AMovieID: string);
begin
  inherited;
  InfoPageEncoding := peUTF8;
  FlashVarsRegExp := RegExCreate(REGEXP_FLASHVARS);
  FlashVarsItemsRegExp := RegExCreate(REGEXP_FLASHVARS_ITEMS);
  FlashVarsItemsQualityRegExp := RegExCreate(REGEXP_FLASHVARS_ITEMS_QUALITY);
end;

destructor TDownloader_PornHub.Destroy;
begin
  RegExFreeAndNil(FlashVarsRegExp);
  RegExFreeAndNil(FlashVarsItemsRegExp);
  RegExFreeAndNil(FlashVarsItemsQualityRegExp);
  inherited;
end;

function TDownloader_PornHub.GetMovieInfoUrl: string;
begin
  Result := 'http://www.pornhub.com/view_video.php?viewkey=' + MovieID;
end;

function TDownloader_PornHub.AfterPrepareFromPage(var Page: string; PageXml: TXmlDoc; Http: THttpSend): boolean;
var
  FlashVars, Title, Url, Encrypted, BestUrl, sQuality: string;
  BestQuality, Quality: integer;
begin
  inherited AfterPrepareFromPage(Page, PageXml, Http);
  Result := False;
  if not GetRegExpVar(FlashVarsRegExp, Page, 'FLASHVARS', FlashVars) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else if not GetRegExpVarPairs(FlashVarsItemsRegExp, FlashVars, ['video_title', 'video_url', 'encrypted'], [@Title, @Url, @Encrypted]) then
    SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_INFO)
  else
    begin
    BestUrl := Url;
    if BestUrl = '' then
      begin
      BestQuality := -1;
      if GetRegExpVars(FlashVarsItemsQualityRegExp, FlashVars, ['QUALITY', 'URL'], [@sQuality, @Url]) then
        repeat
          if Url <> '' then
            begin
            Quality := StrToIntDef(sQuality, 0);
            if Quality > BestQuality then
              begin
              BestUrl := Url;
              BestQuality := Quality;
              end;
            end;
        until not GetRegExpVarsAgain(FlashVarsItemsQualityRegExp, ['QUALITY', 'URL'], [@sQuality, @Url]);
      end;
    Url := UrlDecode(BestUrl);
    if Url <> '' then
      if AnsiCompareText(Encrypted, 'true') = 0 then
        if Title <> '' then
          Url := {$IFDEF UNICODE} string {$ENDIF} (Decrypt(DecodeBase64( {$IFDEF UNICODE} AnsiString {$ENDIF} (Url)), {$IFDEF UNICODE} AnsiString {$ENDIF} (StringToUtf8(StringReplace(Title, '+', ' ', [rfReplaceAll])))));
    if not IsHttpProtocol(Url) then
      SetLastErrorMsg(ERR_FAILED_TO_LOCATE_MEDIA_URL)
    else
      begin
      MovieUrl := Url;
      SetName(UrlDecode(Title));
      SetPrepared(True);
      Result := True;
      end;
    end;
end;

function TDownloader_PornHub.Decrypt(const Data, Password: AnsiString): AnsiString;
begin
  // The URL is encrypted by AES256 in CTR mode, where the first block is the
  // initial CTR value.
  // For further details, search the Flash for _getDecryptedVideoUrl.
  Result := AESCTR_Decrypt(Data, Password, 256);
end;

initialization
  {$IFDEF XXX}
  RegisterDownloader(TDownloader_PornHub);
  {$ENDIF}

end.
