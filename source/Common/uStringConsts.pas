(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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

unit uStringConsts;
{$INCLUDE 'YTD.inc'}

interface

const
  REGEXP_COMMON_URL = '^https?://(?:[a-z0-9-]+\.)*%s(?P<%s>%s)%s';
    // Protocol HTTP or HTTPS, any number of subdomains, pre-ID, downloader class, ID, post-ID
  REGEXP_SOMETHING = '.+';
  REGEXP_NUMBERS = '[0-9]+';
  REGEXP_PATH_COMPONENT = '[^/?&]+';

  // Common regular expressions for getting Title
  REGEXP_TITLE_TITLE = '<title>\s*(?P<TITLE>.*?)\s*</title>';
  REGEXP_TITLE_DIV_CLASS = '<div\s+class="%s">\s*(?P<TITLE>.*?)\s*</div>';
  REGEXP_TITLE_SPAN_CLASS = '<span\s+class="%s">\s*(?P<TITLE>.*?)\s*</span>';
  REGEXP_TITLE_META_TITLE = '<meta\s+name="title"\s+content="\s*(?P<TITLE>.*?)\s*"';
  REGEXP_TITLE_H1 = '<h1[^>]*>\s*(?P<TITLE>.*?)\s*</h1>';
  REGEXP_TITLE_H1_CLASS = '<h1\s+class="%s">\s*(?P<TITLE>.*?)\s*</h1>';
  REGEXP_TITLE_H2 = '<h2[^>]*>\s*(?P<TITLE>.*?)\s*</h2>';
  REGEXP_TITLE_H2_CLASS = '<h2\s+class="%s">\s*(?P<TITLE>.*?)\s*</h2>';
  REGEXP_TITLE_H3 = '<h3[^>]*>\s*(?P<TITLE>.*?)\s*</h3>';
  REGEXP_TITLE_H3_CLASS = '<h3\s+class="%s">\s*(?P<TITLE>.*?)\s*</h3>';

  // Common regular expressions for getting Url
  REGEXP_URL_EMBED_SRC = '<embed\s+src="(?P<URL>https?://.+?)"';
  REGEXP_URL_PARAM_MOVIE = '<param\s+name="movie"\s+value="(?P<URL>.+?)"';
  REGEXP_URL_LINK_VIDEOSRC = '<link\s+rel="video_src"\s+href="(?P<URL>https?://.+?)"';

  HTTP_FORM_URLENCODING = 'application/x-www-form-urlencoded';
  HTTP_FORM_URLENCODING_UTF8 = HTTP_FORM_URLENCODING + '; charset=UTF-8';

  URL_QUERY_VARS = '[?&](?P<VARNAME>[^=]+)=(?P<VARVALUE>[^&]*)';

  FLASH_DEFAULT_VERSION = 'WIN 10,1,82,76';

implementation

end.
