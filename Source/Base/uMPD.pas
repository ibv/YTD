(******************************************************************************

______________________________________________________________________________

YTD v1.64                                                    (c) 2019  ibv
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
      fAudioList: TList;
      fVideoList: TList;
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

      property AudioList: TList read fAudioList;
      property VideoList: TList read fVideoList;
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
  fAudioList:=TList.Create;
  fVideoList:=TList.Create;
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
  FreeAndNil(fAudioList);
  FreeAndNil(fVideoList);
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
    if XmlNodeByPath(Node1,'SegmentTemplate',Node) then
    begin
      fmedia := XmlAttribute(Node, 'media');
      finit := XmlAttribute(Node, 'initialization');
      fVideostartNumber := Node.ReadAttributeInteger('startNumber',0);
    end;

    Node1.FindNodes('S',fVideoList);
    fVideoEndNumber:=0;
    for i:=0 to fVideoList.Count-1 do
    begin
      if TXMLNode(fVideoList[i]).ReadAttributeInteger('r',0) > 0 then
      begin
         fVideoEndNumber := TXMLNode(fVideoList[i]).ReadAttributeInteger('r',0);
         fVTimeT := TXMLNode(fVideoList[i]).ReadAttributeInteger('t',0);
         fVTimeD := TXMLNode(fVideoList[i]).ReadAttributeInteger('d',0);
      end;
    end;
    inc(fVideoEndNumber,fVideoList.count-1);

    Node1.FindNodes('Representation',fVideoList);
  end;

  if fxml.NodeByPathAndAttr('Period/AdaptationSet','mimeType','audio/mp4',Node1) then
  begin
    lang := XmlAttribute(Node1, 'lang');

    if XmlNodeByPath(Node1,'SegmentTemplate',Node) then
    begin
      famedia := XmlAttribute(Node, 'media');
      fainit := XmlAttribute(Node, 'initialization');
      fAudiostartNumber := Node.ReadAttributeInteger('startNumber',0);
    end;

    Node1.FindNodes('S',fAudioList);
    fAudioEndNumber := 0;
    for i:=0 to fAudioList.Count-1 do
    begin
      if TXMLNode(fAudioList[i]).ReadAttributeInteger('r',0) > 0 then
      begin
         fAudioEndNumber := TXMLNode(fAudioList[i]).ReadAttributeInteger('r',0);
         fATimeT := TXMLNode(fAudioList[i]).ReadAttributeInteger('t',0);
         fATimeD := TXMLNode(fAudioList[i]).ReadAttributeInteger('d',0);
      end;
    end;
    inc(fAudioEndNumber,fAudioList.count-1);

    Node1.FindNodes('Representation',fAudioList);

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
  List:=fVideoList;
  media:=fmedia;
  init :=finit;
  if not Video then
  begin
    List:=fAudioList;
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
  // time based segment
  if AnsiContainsStr(media, '$Time') then
  begin
    // ToDo
  end
  else
  // id based segment
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
