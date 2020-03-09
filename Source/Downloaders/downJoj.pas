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

unit downJoj;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  uPCRE, uXml, uCompatibility, HttpSend, SynaCode,
  uOptions,
  {$IFDEF GUI}
    guiDownloaderOptions,
    {$IFDEF GUI_WINAPI}
      guiOptionsWINAPI_Joj,
    {$ELSE}
      {$IFNDEF GUI_LCL}
        guiOptionsVCL_Joj,
      {$ELSE}
  		  guiOptionsLCL_Joj,
  		{$ENDIF}
    {$ENDIF}
  {$ENDIF}
  uDownloader, uCommonDownloader, uDummyDownloader;

type
  TDownloader_Joj = class(TDummyDownloader)
    private
    protected
    public
      class function Provider: string; override;
      class function UrlRegExp: string; override;
      class function Features: TDownloaderFeatures; override;
      {$IFDEF GUI}
      class function GuiOptionsClass: TFrameDownloaderOptionsPageClass; override;
      {$ENDIF}
    end;

const
  OPTION_JOJ_SERVER {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'server';
  OPTION_JOJ_SERVER_DEFAULT {$IFDEF MINIMIZESIZE} : string {$ENDIF} = 'n15.joj.sk';

implementation

uses
  uStringConsts,
  uDownloadClassifier,
  uMessages;

// http://www.joj.sk/anosefe/anosefe-epizody/2011-05-09-ano-sefe-.html
// http://www.joj.sk/sudna-sien/sudna-sien-archiv/2011-05-03-sudna-sien.html
const
  URLREGEXP_BEFORE_ID = '';
  URLREGEXP_ID =        REGEXP_COMMON_URL_PREFIX + 'joj\.sk/' + REGEXP_SOMETHING;
  URLREGEXP_AFTER_ID =  '';

const
  REGEXP_MOVIE_TITLE = REGEXP_TITLE_TITLE;
  REGEXP_MOVIE_ID = '<div\b[^>]*\sdata-id="(?P<ID>[a-f0-9-]+)"[^>]*\sdata-pageid="(?P<PAGEID>[A-Za-z0-9+/=]+)"[^>]*';

{ TDownloader_Joj }

class function TDownloader_Joj.Provider: string;
begin
  Result := 'Joj.sk';
end;

class function TDownloader_Joj.UrlRegExp: string;
begin
  Result := Format(REGEXP_BASE_URL, [URLREGEXP_BEFORE_ID, MovieIDParamName, URLREGEXP_ID, URLREGEXP_AFTER_ID]);
end;

class function TDownloader_Joj.Features: TDownloaderFeatures;
begin
  Result := inherited Features + [dfAcceptSecureToken];
end;

{$IFDEF GUI}
class function TDownloader_Joj.GuiOptionsClass: TFrameDownloaderOptionsPageClass;
begin
  Result := TFrameDownloaderOptionsPage_Joj;
end;
{$ENDIF}

initialization
  RegisterDownloader(TDownloader_Joj);

end.
