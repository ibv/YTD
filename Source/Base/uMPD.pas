(******************************************************************************

______________________________________________________________________________

YTD v1.69                                                    (c) 2023  ibv
https://ibv.github.io/YTD/
______________________________________________________________________________


Copyright (c) 201 ibv (https://ibv.github.io/YTD/)
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

unit uMPD;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes,
  {$ifdef mswindows}
    Windows,
  {$ENDIF}
  {$IFDEF fpc}
  LCLIntf, LCLType, 
  {$ENDIF}
  uXml;

type
  TMPDObject = class
    private
      fBaseURL:  String;
      fAudioRepreList: TList;
      fVideoRepreList: TList;
      fVideoSegmentList: TList;
      fAudioSegmentList: TList;
      fXml: TXmlDoc;
      fMedia,fInit,
      fAMedia,fAInit: string;
      fAudioStartNumber,
      fAudioEndNumber,
      fVideoStartNumber,
      fVideoEndNumber,
      fVTimeT,fATimeT,
      fVTimeD,fAtimeD: integer;

      procedure ParseMPD;
    public
      fm:  string;
      constructor Create(Xml: TXmlDoc);
      destructor Destroy;

      function GetBestID(BandWidth:integer;Video:boolean = true):string;

      property AudioSegmenList: TList read fAudioSegmentList;
      property VideoSegmentList: TList read fVideoSegmentList;
      property BaseURL: String read fBaseURL write fBaseURL;
      property VideoMedia: string read fMedia;
      property VideoInit: string read fInit;
      property VideoStartNumber: integer read fVideoStartNumber;
      property VideoEndNumber: integer read fVideoEndNumber;
      property VideoTimeT: integer read fVTimeT;
      property VideoTimeD: integer read fVTimeD;
      property AudioMedia: string read fAMedia;
      property AudioInit: string read fAInit;
      property AudioStartNumber: integer read fAudioStartNumber;
      property AudioEndNumber: integer read fAudioEndNumber;
      property AudioTimeT: integer read fATimeT;
      property AudioTimeD: integer read fATimeD;

  end;



implementation

uses
  strutils;



{ TMPDObject }

constructor TMPDObject.Create(Xml:TXmlDoc);
begin
  fXml := Xml;
  fAudioRepreList:=TList.Create;
  fVideoRepreList:=TList.Create;
  fAudioSegmentList:=TList.Create;
  fVideoSegmentList:=TList.Create;
  fBaseURL := '';
  fMedia:='';
  fInit:='';
  fAMedia:='';
  fAInit:='';
  fVideoStartNumber:=0;
  fVideoEndNumber:=0;
  fAudioStartNumber:=0;
  fAudioEndNumber:=0;
  fVTimeT:=0;
  fVTimeD:=0;
  fATimeT:=0;
  fAtimeD:=0;

  ParseMPD;
end;


destructor TMPDObject.Destroy;
begin
  FreeAndNil(fAudioRepreList);
  FreeAndNil(fVideoRepreList);
  FreeAndNil(fAudioSegmentList);
  FreeAndNil(fVideoSegmentList);
end;


procedure TMPDObject.ParseMPD;
var
  Node,Node1 : TXmlNode;
  lang: string;
  i: integer;
begin
  if fXml.NodeByPath('BaseURL', Node) then
      BaseURL := XmlValueIncludingCData(Node);

  if fxml.NodeByPathAndAttr('Period/AdaptationSet','mimeType','video/mp4',Node1) then
  begin
    if XmlNodeByPath(Node1,'SegmentTemplate',Node) or XmlNodeByPath(Node1,'Representation/SegmentTemplate',Node) then
    begin
      fmedia := XmlAttribute(Node, 'media');
      finit := XmlAttribute(Node, 'initialization');
      fVideostartNumber := Node.ReadAttributeInteger('startNumber',0);
    end;

    Node1.FindNodes('S',fVideoSegmentList);
    fVideoEndNumber:=0;
    // id based segment
    if not AnsiContainsStr(fMedia, '$Time$') then
    begin
      for i:=0 to fVideoSegmentList.Count-1 do
      begin
        ///if TXMLNode(fVideoSegmentList[i]).ReadAttributeInteger('r',0) > 0 then
          inc(fVideoEndNumber,TXMLNode(fVideoSegmentList[i]).ReadAttributeInteger('r',0));
      end;
    end;
    inc(fVideoEndNumber,fVideoSegmentList.count-1);

    Node1.FindNodes('Representation',fVideoRepreList);
  end;

  if fxml.NodeByPathAndAttr('Period/AdaptationSet','mimeType','audio/mp4',Node1) then
  begin
    lang := XmlAttribute(Node1, 'lang');

    if XmlNodeByPath(Node1,'SegmentTemplate',Node) or XmlNodeByPath(Node1,'Representation/SegmentTemplate',Node) then
    begin
      famedia := XmlAttribute(Node, 'media');
      fainit := XmlAttribute(Node, 'initialization');
      fAudiostartNumber := Node.ReadAttributeInteger('startNumber',0);
    end;

    Node1.FindNodes('S',fAudioSegmentList);
    fAudioEndNumber := 0;
    // id based segment
    if not AnsiContainsStr(faMedia, '$Time$') then
    begin
      for i:=0 to fAudioSegmentList.Count-1 do
      begin
        ///if TXMLNode(fAudioSegmentList[i]).ReadAttributeInteger('r',0) > 0 then
          inc(fAudioEndNumber,TXMLNode(fAudioSegmentList[i]).ReadAttributeInteger('r',0));
      end;
    end;
    inc(fAudioEndNumber,fAudioSegmentList.count-1);

    Node1.FindNodes('Representation',fAudioRepreList);

  end;
end;


function TMPDObject.GetBestID(BandWidth:integer;Video:boolean = true):string;
var
  i,j,k,quality: integer;
  id,media,init:string;
  Node: TXmlNode;
  List: TList;
begin
  result:='';
  List:=fVideoRepreList;
  media:=fmedia;
  init :=finit;
  if not Video then
  begin
    List:=fAudioRepreList;
    media:=famedia;
    init:=fainit;
  end;
  for i:=0 to List.Count-1 do
  begin
     Node := TXMLNode(List[i]);
     id := XmlAttribute(node, 'id');
     Quality := Node.ReadAttributeInteger('bandwidth',0);
     if (BandWidth-Quality <= 0)  then  break;
  end;
  if id <> '' then result:=id;

  Init := StringReplace(init,'$RepresentationID$',id,[]);
  Media := StringReplace(media,'$RepresentationID$',id,[]);
  // id based segment, CT
  if AnsiContainsStr(media, '$Number') then
  begin
    //%06d$
    media := StringReplace(media,'$Number','',[]);
    j := pos('%',media);
    k := LastDelimiter('$', media);
    fm := copy(media,j+1,k-j-1);
  end;

  if Video then
  begin
    fmedia:=media;
    finit:=init;
  end
  else
  begin
    famedia:=media;
    fainit:=init;
  end;


end;





end.
