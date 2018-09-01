unit janXMLParser2;

interface
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  Windows, ComObj, Contnrs, Messages, SysUtils, Classes, {$IFDEF FPC} Variants, {$ELSE} Graphics, Controls, Forms, Menus, {$ENDIF} Dialogs,janXPathTokenizer, Math, janstrings;

const
  delimiters=['+','-','*','/',' ','(',')','=','>','<'];
  numberchars=['0'..'9','.'];
  identchars=['a'..'z','A'..'Z','0'..'9','.','_'];

  {** This constant controls the initial size of the hash. }
  c_HashInitialItemShift = 7;

  {** How inefficient do we have to be before we automatically Compact? }
  c_HashCompactR         = 2;   { This many spaces per item. }
  c_HashCompactM         = 100; { Never for less than this number of spaces. }

type
  {** General exception classes. }
  EHashError = class(Exception);
  EHashErrorClass = class of EHashError;

  {** Exception for when an item is not found. }
  EHashFindError = class(EHashError);

  {** Exception for invalid Next op. }
  EHashIterateError = class(EHashError);

  {** Exception for invalid keys. }
  EHashInvalidKeyError = class(EHashError);

  {** Record, should really be private but OP won't let us... }
  THashRecord = record
    Hash: Cardinal;
    ItemIndex: integer;
    Key: string;
  end;

  {** Iterator Record. This should also be private. This makes me almost like
      the way Java does things. Almost. Maybe. }
  THashIterator = record
    ck, cx: integer;
  end;

  {** Base Hash class. Don't use this directly. }
  THash = class
    protected
      {** The keys. }
      f_Keys: array of array of THashRecord;

      {** Current bucket shift. }
      f_CurrentItemShift: integer;

      {** These are calculated from f_CurrentItemShift. }
      f_CurrentItemCount: integer;
      f_CurrentItemMask: integer;
      f_CurrentItemMaxIdx: integer;

      {** Spare items. }
      f_SpareItems: array of integer;

      {** Whether Next is allowed. }
      f_NextAllowed: boolean;

      {** Current key. }
      f_CurrentKey: string;

      {** Can we compact? }
      f_AllowCompact: boolean;

      {** Our current iterator. }
      f_CurrentIterator: THashIterator;

      {** Update the masks. }
      procedure FUpdateMasks;

      {** Update the buckets. }
      procedure FUpdateBuckets;

      {** Find a key's location. }
      function FFindKey(const Key: string; var k, x: integer): boolean;

      {** Add a new key, or change an existing one. Don't call this directly. }
      procedure FSetOrAddKey(const Key: string; ItemIndex: integer);

      {** Abstract method, delete value with a given index. Override this. }
      procedure FDeleteIndex(i: integer); virtual; abstract;

      {** Get the number of items. }
      function FGetItemCount: integer;

      {** Allocate an item index. }
      function FAllocItemIndex: integer;

      {** Abstract method, move an item with index OldIndex to NewIndex.
          Override this. }
      procedure FMoveIndex(oldIndex, newIndex: integer); virtual; abstract;

      {** Abstract method, trim the indexes down to count items. Override
          this. }
      procedure FTrimIndexes(count: integer); virtual; abstract;

      {** Abstract method, clear all items. Override this. }
      procedure FClearItems; virtual; abstract;

      {** Tell us where to start our compact count from. Override this. }
      function FIndexMax: integer; virtual; abstract;

      {** Compact, but only if we're inefficient. }
      procedure FAutoCompact;

    public
      {** Our own constructor. }
      constructor Create; reintroduce; virtual;

      {** Does a key exist? }
      function Exists(const Key: string): boolean;

      {** Rename a key. }
      procedure Rename(const Key, NewName: string);

      {** Delete a key. }
      procedure Delete(const Key: string);

      {** Reset iterator. }
      procedure Restart;

      {** Next key. }
      function Next: boolean;

      {** Previous key. }
      function Previous: boolean;

      {** Current key. }
      function CurrentKey: string;

      {** The number of items. }
      property ItemCount: integer read FGetItemCount;

      {** Compact the hash. }
      procedure Compact;

      {** Clear the hash. }
      procedure Clear;

      {** Allow compacting? }
      property AllowCompact: boolean read f_AllowCompact write f_AllowCompact;

      {** Current iterator. }
      property CurrentIterator: THashIterator read f_CurrentIterator write
        f_CurrentIterator;

      {** Create a new iterator. }
      function NewIterator: THashIterator;
  end;

    {** Hash of objects. }
  TObjectHash = class(THash)
    private
      FOwnsItems: boolean;
      procedure SetOwnsItems(const Value: boolean);
    protected
      {** The index items. }
      f_Items: array of TObject;

      {** Override FDeleteIndex abstract method. }
      procedure FDeleteIndex(i: integer); override;

      {** Get an item or raise an exception. }
      function FGetItem(const Key: string): TObject;

      {** Set or add an item. }
      procedure FSetItem(const Key: string; Value: TObject);

      {** Move an index. }
      procedure FMoveIndex(oldIndex, newIndex: integer); override;

      {** Trim. }
      procedure FTrimIndexes(count: integer); override;

      {** Clear all items. }
      procedure FClearItems; override;

      {** Where to start our compact count from. }
      function FIndexMax: integer; override;

    public
      {** Items property. }
      property Items[const Key: string]: TObject read FGetItem
        write FSetItem; default;
      property OwnsItems:boolean read FOwnsItems write SetOwnsItems;
      {** Destructor must destroy all items. }
      destructor Destroy; override;

  end;



  TjanXMLNode2=class;

  TVariableEvent=procedure(sender:Tobject;const VariableName:string;var VariableValue:variant;var handled:boolean) of object;
  TXELEvent=procedure(sender:TObject;node:TjanXMLNode2;const Act:string) of object;
  TjanXSLSort=record
    pattern:string;
    SortAscending:boolean;
    SortNumeric:boolean;
  end;


  TjanXMLNodeList2=class(TList)
  protected
  public
  {TList descendant that frees the referenced objects.}
    procedure Clear;override;
    {Clears list and frees referenced objects}
    destructor destroy; override;
    {Clears before destroy}
  end;

  TjanXPathParserList2=class(TStringList)
  public
  {TStringList descendant that frees the referenced objects.}
    procedure Clear;override;
    {Clears list and frees referenced objects}
    destructor destroy; override;
    {Clears before destroy}
  end;

  TjanXPathExpression2=class;

  TjanXMLParser2=class;
  TjanXPathParser2=class;



  TjanXMLAttribute2=class(TObject)
  private
    Fvalue: variant;
    Fname: string;
    procedure Setname(const Value: string);
    procedure Setvalue(const Value: variant);
  public
  {XML Attribute object}
    property name:string read Fname write Setname;
    {Holds the name of the attribute.}
    property value:variant read Fvalue write Setvalue;
    {Holds the string value of the attribute}
    function cloneAttribute:TjanXMLAttribute2;
    {Returns a copy of the attribute object.}
  end;


  TjanXPathParser2=class(TObject)
  private
    Fpattern: string;
    FXPath:TjanXPathExpression2;
    FCurrentNode: TjanXMLNode2;
    procedure Setpattern(const Value: string);
    procedure SetCurrentNode(const Value: TjanXMLNode2);
  public
  {Helper object to handle QXML expressions to select nodes.}
    constructor Create;
    {Creates XPath expression object}
    destructor  destroy; override;
    {Destroys XPath expression object}
    procedure selectNodes(node:TjanXMLNode2;nodelist:TList;single:boolean=false);
    {Creates a list of recursive child nodes of a given node and test each node against the given QXML expression.
     Matching nodes are added to nodelist.}
    procedure filterNodes(nodelist:TList);
    {filters a list of nodes against the given QXML expression.
     Non-Matching nodes are removed from nodelist.}
    function testNode(node:TjanXMLNode2):boolean;
    {Tests if a single node matches the pattern}
    property pattern:string read Fpattern write Setpattern;
    {Determines the QXML query expression.}
    property XPath:TjanXPathExpression2 read FXPath;
    {The expression parser and evaluator.}
    property CurrentNode:TjanXMLNode2 read FCurrentNode write SetCurrentNode;
    {Determines the node that is used when evaluating the expression.}
  end;


  TjanXMLNode2=class(TObject)
  private
    Fname:string;
    Fnamespace:string;
    Ftext:string;
    FCDATA:boolean;
    FParser:TjanXMLParser2;
    FParentNode:TjanXMLNode2;
    FNodes:TjanXMLNodeList2;
    FAttributes:TjanXMLNodeList2;
//    Fscan:integer;
    procedure Settext(const Value: string);
    procedure Setname(const Value: string);
    function getAttribute(index: variant): variant;
    procedure setAttribute(index: variant; const Value: variant);
    procedure AddParsedAttribute(pName,pValue:string);
    function getAttributeCount: integer;
    function Getattributename(index: integer): string;
    procedure SetParentNode(const Value: TjanXMLNode2);
    function GetID: string;
    procedure SetID(const Value: string);
    function xelSwitch(anode:TjanXMLNode2;parser:TjanXMLParser2):boolean;
    function GetchildNode(index: integer): TjanXMLNode2;
    function GetRootNode: TjanXMLNode2;
    procedure RemoveNodeIndexes;
    function getAsFloat(index: variant): extended;
    function getAsInteger(index: variant): integer;
    function getAsBoolean(index: variant): boolean;
    function getAsDateTime(index: variant): TDateTime;
    function getAsText(index: variant): string;
   protected
  public
    {The basic object of janXML}
    constructor create;
    destructor  destroy; override;
    property parentNode:TjanXMLNode2 read FParentNode write SetParentNode;
    {Refers to the parent node.}
    property name:string read Fname write Setname;
    {Indicates the name of the node.}
    property namespace:string read Fnamespace;
    {Indicates the namespace of the node. This property is readonly and implicitly set via the name property.}
    property CDATA:boolean read FCDATA;
    {Indicates if the node text is character data containing > or <. This property is readonly and implicitly set via the text property.}
    property text:string read Ftext write Settext;
    {Holds the text value of the node.}
    property attribute[index:variant]: variant read getAttribute write setAttribute;default;
    {Provides indexed access to attributes of the node.
    This is the default property of TjanXMLNode2, so you can use anode['price']}
    property attributecount:integer read getAttributeCount;
    {Indicates the number of attrbutes.}
    property attributename[index:integer]:string read Getattributename;
    {Provides indexed access to the attribute names.}
    property AsText[index:variant]: string read getAsText;
    {Provides indexed access to attributes of the node, converting between floating point value and string. Invalid strings are converted to 0}
    property AsFloat[index:variant]: extended read getAsFloat;
    {Provides indexed access to attributes of the node, converting between floating point value and string. Invalid strings are converted to 0}
    property AsInteger[index:variant]: integer read getAsInteger;
    {Provides indexed access to attributes of the node, converting between integer value and string. Invalid strings are converted to 0.}
    property AsBoolean[index:variant]: boolean read getAsBoolean;
    {Provides indexed access to attributes of the node, converting between boolean value and string. Uses '0' for False and '1' for True.}
    property AsDateTime[index:variant]: TDateTime read getAsDateTime;
    {Provides indexed access to attributes of the node, converting between datetime value and string. Invalid strings return 0.}
    property nodes:TjanXMLNodeList2 read FNodes;
    {Returns a reference to the list of child nodes.}
    property childNode[index:integer]:TjanXMLNode2 read GetchildNode;
    {Returns child node at index or nil if out of range.}
    property attributes:TjanXMLNodeList2 read FAttributes;
    {Returns a reference to the list of attributes.}
    property id:string read GetID write SetID;
    {Shortcut for attribute['id']}
    property rootNode:TjanXMLNode2 read GetRootNode;
    function hasAttribute(aname:string):boolean;
    {Determines if a given attribute exists.}
    procedure addNode(node:TjanXMLNode2);
    {Adds a child node.}
    procedure deleteNode(node:TjanXMLNode2);
    {Delete a given child node}
    procedure insertNode(index:integer;node:TjanXMLNode2);
    {Insert childnode at given position}
    function indexOfAttribute(aname:string):integer;
    {Returns the index of a named attribute.}
    function deleteAttribute(attribute:TjanXMLAttribute2):boolean;overload;
    {Deletes a given attribute.}
    function deleteAttribute(attributeName:string):boolean;overload;
    {Deletes a given attribute by name.}
    function renameAttribute(oldname,newname:string):boolean;
    {Renames a named attribute.}
    function moveto(node:TjanXMLNode2):boolean;
    {Adds the node as child node to a given node.}
    function cloneNode:TjanXMLNode2;
    {Returns a recursive copy of the node.}
    procedure selectNodes(nodelist: TList; pattern:string;single:boolean=false);overload;
    {Adds recursive child nodes that match pattern to nodelist.}
    procedure selectNodes(parser:TjanXMLParser2;nodelist: TList; pattern:string;single:boolean=false);overload;
    {Adds recursive child nodes that match pattern to nodelist.
    Makes use of the parsers compiled patterns}
    procedure listChildren(alist:TList);
    {Adds recursive child nodes to alist.}
    function childCount:integer;
    {Returns the number of child nodes}
    function namedChildCount(pname:string):integer;
    {Returns the number of child nodes with the given pname.}
    function FirstChild:TjanXMLNode2;
    {Returns first child (if any) or nil}
    function LastChild:TjanXMLNode2;
    {Returns last child (if any) or nil}
    function NextSibling:TjanXMLNode2;
    {Returns next sibling (if any) or nil}
    function PreviousSibling:TjanXMLNode2;
    {Returns previous sibling (if any) or nil}
    function getParentByName(aname:string):TjanXMLNode2;
    {Find first (recursive) parentnode with the given name}
    function addChildByName(aname:string):TjanXMLNode2;
    {Adds a child with the given tagname}
    function forceChildByName(aname:string):TjanXMLNode2;
    {Returns the first child node with the given tagname and create this node when not present}
    function getChildByName(aname:string; descent:boolean=false):TjanXMLNode2;
    {Returns the first child node with the given tagname}
    procedure getChildrenByName(aname:string;nodelist:TList;descent:boolean=false);
    {Returns a list of (recursive) child nodes with the given name}
    function getChildByPath(apath:string):TjanXMLNode2;
    {Returns the first (descendant node with the given path: child1/child2 etc}
    function forceChildByPath(apath:string):TjanXMLNode2;
    {Returns (and forces when required) the first (descendant node with the given path: child1/child2 etc}
    function getChildByID(aid:string;descent:boolean=false):TjanXMLNode2;
    {Returns the first child node with the given id attribute value}
    function getChildByAttribute(AttributeName,AttributeValue:string):TjanXMLNode2;
    {Returns the first child node where the given attributename has attributevalue}
    function getChildText(pName:string):string;
    {Returns the text of the first child node with the given tagname; if not found an empty string is returned}
    procedure getHtmlOptions(list:Tstringlist;path:string);
    {returns a list of <option> tags with value= node['id'] and text = node.text
    each new level add path to the text, e.g. --}
    function transformNode(stylesheet:TjanXMLparser2):string;
    {Transforms the node using stylesheet. Stylesheet must contain an XSLT document.
    The method uses a simplified version of the XSLT standard.
    }
    function namespaceURI:string;
    {Retrieves the value of the matching xmlns:<namespace> value of the node or the first matching parentnode}
    function executeNode(parser:TjanXMLParser2):boolean;
    {Executes a node and any (recursive) child nodes using the script attribute.}
    function getXELVariable(aname:string):TjanXMLNode2;
    {Looks for neares <xel:var id="aname"> and returns the text or an empty string when not found}
    function NodeIndex:integer;
    {Returns the index of the node in the nodelist of its parent node, returns -1 if not parent node}
  end;



  TjanXMLParser2 = class(TjanXMLNode2)
  private
    { Private declarations }
    Fxml:string;
    Fscan:integer;
    FparseRoot:TjanXMLNode2;
    Foutput:string;
    FOutputDepth:integer;
    FXSLAtom:string;
    FXMLSize:integer;
    FXMLPosition:integer;
    FXMLP: PChar;
    FparseError: string;
    Fdeclaration: TjanXMLNode2;
    FPatterns:TjanXPathParserList2;
    FpageSize: integer;
    FonXEL: TXELEvent;
    FNodeIndex:TObjectHash;
    FIndexesByNameAndId:TObjectHash;
    FIsParsing: boolean;
    FAutoIndexByNameAndId: boolean;
    FIsDestroying:boolean;
    //FEscapeAttributes: boolean;
    function getXML: string;
    procedure setXML(const Value: string); virtual;
    function AsText:string;
    procedure OutputNode(node:TjanXMLNode2);
    procedure XSLTOutputNode(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    procedure ExecXSLT(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    procedure xsl_value_of(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    procedure xsl_call_template(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    procedure xsl_apply_templates(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    procedure xsl_if(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    procedure xsl_attribute(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    procedure xsl_element(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    procedure xsl_comment(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    procedure xsl_choose(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    procedure xsl_for_each(node,context:TjanXMLNode2;matchlist,templatelist:TList);
    function cdatacheck(value:string):string;
    function parse:string;
    function parseFragment(pNode:TjanXMLNode2):string;
    function parseNode(rootNode,parentNode:TjanXMLNode2):boolean;
    procedure parseAttributes(node:TjanXMLNode2;atts:string);
    procedure parseText(node:TjanXMLNode2);
    procedure XMLPut(value: string);
    procedure SetpageSize(const Value: integer);
    procedure Sort(matchlist:TList;From, Count: Integer;orderby:array of TjanXSLSort);
    function Compare(matchlist:TList;i, j: Integer;orderby:array of TjanXSLSort): Integer;
    procedure Swap(matchlist:TList;i, j: Integer);
    procedure SetonXEL(const Value: TXELEvent);
    function getNodeXML(pNode: TjanXMLNode2): string;
    procedure setNodeXML(pNode: TjanXMLNode2; const Value: string);
    procedure IndexNode_(pNode:TjanXMLNode2);
    procedure IndexNodeByNameAndId_(pNode:TjanXMLNode2);
    procedure NewNodeFromSchema(Schema:TjanXMLParser2;docNode,schemaNode:TjanXMLNode2);
    procedure SetAutoIndexByNameAndId(const Value: boolean);
    //procedure SetEscapeAttributes(const Value: boolean);
  protected
    { Protected declarations }
    function transformNode_(node:TjanXMLNode2):string;
    {transforms the node using XSLT stylesheet}
  public
    {TjanXMLParser2 is W3C compliant XML parser supporting XSLT transformations.

    Non-standard:
    Supports auto-indexing of nodes by nodeName and id attribute.
    This allows superfast indexed access to any node using the GetNodeByNameAndId method.

    }
    { Public declarations }
    constructor create;virtual;
    destructor  destroy;override;
    {A descendant of TjanXMLNode2 with some added properties and methods for persisting XML}
    procedure LoadXML(filename:string); virtual;
    {Loads an XML document from filename and parses the document into a DOM.}
    procedure SaveXML(filename:string);
    {Saves the DOM to filename.}
    procedure IndexNodes;
    {Index unique id attribute of nodes, will raise an exception when a duplicate id is encountered. Only nodes with id attribute are indexed}
    procedure IndexNodesByNameAndId;
    {Index unique id attribute of nodes, grouped by nodename; will raise an exception when a duplicate id is encountered. Only nodes with id attribute are indexed}
    procedure IndexNodeByNameAndId(pNode:TjanXMLNode2);
    {Index unique id attribute of pNode, grouped by nodename; will raise an exception when a duplicate id is encountered. Only a node with id attribute are indexed}
    procedure RemoveNodeFromIndexByNameAndId(pNode:TjanXMLNode2);
    {remove index unique id attribute of pNode, grouped by nodename.}
    function NodeFromId(pId:string):TjanXMLNode2;
    {Returns the node with the given id. Can only be called after IndexNodes}
    function GetNodeByNameAndId(pNodeName, pId:string):TjanXMLNode2;
    {Returns the node with the given pNodeName and pId. Can only be called after IndexNodesNamed}
    function DeleteNodeByNameAndId(pNodeName,pId:string):boolean;
    {Delete the node with the given pNodeName and id pId. Returns true when succeeded.}
    function AddNodeByNameAndId(pNodeName,pId:string;pNode:TjanXMLNode2):boolean;
    {Adds pNode as child to the node with pNodeName and id=pId. Returns true when succeeded.}
    procedure NewDocumentFromSchema(Schema:TjanXMLParser2);
    {Create new document from schema}
    function Validate(XsdSchema:TjanXMLParser2):boolean;
    {Validates document against a XSD schema. XsdSchema must contain a valid XSD schema document.}
    function ValidateNode(XsdSchema:TjanXMLParser2;node:TjanXMLNode2):boolean;
    {Validates node against a XSD schema. XsdSchema must contain a valid XSD schema document.}
    function getXPathParser(pattern:string):TjanXPathParser2;
    {Returns a reference to a TjanXPathParser2 instance. The parser keeps an index of used expressions and re-uses existing expressions. This speeds up execution because any expression is only compiled once.}
    function getcomplexType(pType:string):TjanXMLNode2;
    {Returns the <xs:complexType> node with name=pType}
    property NodeXML[pNode:TjanXMLNode2]:string read getNodeXML write setNodeXML;
    {Parses XML fragment into pNode on write and returns XML document fragment on read }
    property xml:string read getXML write setXML;
    {Holds the XML source. Parses on write and returns DOM as text on read.}
    property parseError:string read FparseError;
    {Returns the parse error description.}
    property parsePosition:integer read Fscan;
    {Returns the parse error position in the XML source.}
    property declaration:TjanXMLNode2 read Fdeclaration;
    {Returns the <?xml  .?> declaration if any}
    property pageSize:integer read FpageSize write SetpageSize;
    {Determines the page size for turbo-speed string concatenation. The default = $10000}
    property IsParsing:boolean read FIsParsing;
    {Determines is parser is parsing.}
    property IsDestroying:boolean read FIsDestroying;
    {Indicates of the parser is destroying itself. Is used by child nodes to determine of index changes are needed.}
    property AutoIndexByNameAndId:boolean read FAutoIndexByNameAndId write SetAutoIndexByNameAndId;
    {Determines if Nodes are automatically indexed by NodeName and id.}
    //property EscapeAttributes:boolean read FEscapeAttributes write SetEscapeAttributes;
    //{Determines if attributes are escaped when serializing and unescaped when parsing.}
    property onXEL:TXELEvent read FonXEL write SetonXEL;//event
    {Event is raised when executin a XEL script on a node and a <xel:custom> tag is executed}
  published
    { Published declarations }
  end;

  TjanXSDParser2=class(TjanXMLParser2)
  private
    Types:TObjectHash;
    Elements:TList;
    CurrentType:TjanXMLNode2;
    Paths:TStringList;
    CurrentValidationNode:TjanXMLNode2;
    FEnforceAttributesOnValidation: boolean;
    FEnforceElementsOnValidation: boolean;
    procedure setXML(const Value: string); override;
    procedure Prepare;
    {Prepares for use. Called automatically when loading new content}
    procedure ListTypes(pList:TList);
    procedure ListElements(pList:TList);
    procedure GetChildElements(pNode:TjanXMLNode2;pList:TStringList);
    procedure ListTypeElements(pTypeName:string;pList:TStringList);
    procedure ListElementNames(pNode:TjanXMLNode2;pList:TStringList);
    procedure ListEmbeddedElementNames(pNode:TjanXMLNode2;pList:TStringList);
    procedure TypeWalk(node:TjanXMLNode2;nodepath:string);
    function GetNodePath(node:TjanXMLNode2):string;
    function GetElementType(node:TjanXMLNode2):TjanXMLNode2;
    procedure ListSchemaElements(xt:TjanXMLNode2;pList:TList);
    procedure ListSchemaAttributes(xt:TjanXMLNode2;pList:TList);
    procedure ValidateNodeTypeSequence(pnode, ptypenode, psequencenode: TjanXMLNode2);
    procedure ValidateAttributeRestriction(pnode:TjanXMLNode2;pAttributeName:string; pAttributeValue:string;pRestriction:TjanXMLNode2);
    procedure ValidateNode(pnode:TjanXMLNode2);
    {Validate single node. Any conflict will raise an exception.}
    procedure ValidateNodeType(pnode,ptypenode:TjanXMLNode2);
    {Validate node pnode in document against schema type definition xtypenode.}
    procedure ValidateNodeTypeBasic(pnode:TjanXMLNode2;ptype:string);
    procedure SetEnforceAttributesOnValidation(const Value: boolean);
    procedure SetEnforceElementsOnValidation(const Value: boolean);
    {Validate node pnode in document against basic schema type ptype.}
  public
    {With TjanXSDParser2 you can validate a XML document against a W3C compliant schema.
     The following schema elements are supported:
     <xs:schema>
     <xs:element> :: name, type, minOccurs, maxOccurs
     <xs:attribute> :: name, type, default, use
     <xs:complexType> :: name
     <xs:simpleType> :: name
     <xs:sequence>
     <xs:restriction> :: base
     <xs:enumeration> :: value
     <xs:annotation>
     <xs:documentation>

     type :: xs:string | xs:integer | xs:float | xs:boolean | xs:date | <complexTypeName> | <simpleTypeName>
     xdate :: YYYY-MM-DD (ISO 8601 format)
     
     Limitations:
     Only supports globally defined ComplexType and SimpleType. "Russian doll" method is not supported.

     None-standard:
     type=xs:classid  will automatically generate a classid
    }
    constructor Create;override;
    destructor Destroy;override;
    procedure LoadXML(filename:string); override;
    procedure ValidateDocument(pxmldocument:TjanXMLParser2);
    {Validate complete document. Any conflict will raise an exception.}
    procedure getValidChildElements(pnode:TjanXMLNode2;pList:TList);
    {Get a list of valid child elements bases aon the schema.}
    procedure getValidAttributes(pnode:TjanXMLNode2;pList:TList);
    {Get a list of valid element attributes based aon the schema.}
    function GetType(pnode:TjanXMLNode2):TjanXMLNode2;
    {Get the global type definition of the node.}
    function GetTypeDef(pname:string):TjanXMLNode2;
    {Get the named type global type definition.}
    function GetElement(pnode:TjanXMLNode2):TjanXMLNode2;
    {Get the element definition of the node.}
    function GetDocumentElement:TjanXMLNode2;
    {Get the definition of the document element}
    function GetAttribute(pnode:TjanXMLNode2;pname:string):TjanXMLNode2;
    {Get the xs:attribute given the complexType node}
    procedure ListEnumerations(pnode:TjanXMLNode2;plist:TStringList);
    {Lists the value of any xs:enumeration childnodes of pnode.}
    function CreateElement(pnode:TjanXMLNode2):TjanXMLNode2;
    {Creates element from schema definition pnode, including all required attributes and childnodes.}
    procedure AddAttributes(ptypenode:TjanXMLNode2;var pelementnode:TjanXMLNode2);
    {Add attributes to pelementnode based on attributes defined by ptypenode.}
    function CanDelete(pnode:TjanXMLNode2):boolean;
    {Check if pnode in an instance document can be deleted.}
    function IsValidEnumeration(pNode:TjanXMLNode2;pvalue:string):boolean;
    {Checks if pvalue is equal to one of the xs:enumeration children of pNode.}
    procedure GetWalkPaths(list:TStringList);
    {Debug function that list all node paths.}
    function GetDocumentation(pnode:TjanXMLNode2):string;
    {Returns xs:documentation content of xs:annotation child.}
    procedure ValidateNodeTypeAttribute(pnode, pattributenode: TjanXMLNode2;pAttributeValue:string);
    {Validates attribute value. Use before update. Raises an exception when invalid.}
    function GetSchemaAttribute(pnode:TjanXMLNode2;pAttributeName:string):TjanXMLNode2;
    {Returns the schema xs:attribute node for the given element pnode and attribute.}
    function GetNewPrimaryKey(pDocumentNode:TjanXMLNode2;pAttributeDef:TjanXMLNode2):string;
    {Returns a new unique primarykey for all nodes within the pDocumentNode context, based on pAttributeDef.}
    procedure EnforceClassIds(pDocumentNode:TjanXMLNode2);
    {Ensures that all attributes with type xs:classid have a valid CLASSID value.}
    function GetCurrentValidationNode:TjanXMLNode2;
    {Returns the current node beiing validated}
    function isclassid(value:string):boolean;
    {Tests if value is the string representation of a valid GUID.}
    property EnforceElementsOnValidation:boolean read FEnforceElementsOnValidation write SetEnforceElementsOnValidation;
    {Enforces the precense of all schema required elements when validating. Please note that any present non-schema elements are ignored.}
    property EnforceAttributesOnValidation:boolean read FEnforceAttributesOnValidation write SetEnforceAttributesOnValidation;
    {Enforces the precense of all schema required attributes when validating. Please note that any present non-schema attributes are ignored.}
  end;

  TjanXPathExpression2=class(TObject)
  private
    FInFix:TList;
    FPostFix:TList;
    FStack:TList;
    Fsource: string;
    VStack:array[0..100] of variant;
    SP:integer;
    SL:integer; // source length
//    FToken:string;
//    FTokenKind:TTokenKind;
//    FTokenValue:variant;
//    FTokenOperator:TTokenOperator;
//    FTokenLevel:integer;
//    FTokenExpression:string;
    FPC:integer;
    FonGetVariable: TVariableEvent;
    FCurrentNode: TjanXMLNode2;
    FCurrentList: TList;
    procedure Setsource(const Value: string);
    function Parse:boolean;
//    procedure AddToken;
    procedure ClearInfix;
    procedure ClearPostFix;
    procedure ClearStack;
    function InFixToStack(index:integer):boolean;
    function InfixToPostFix(index:integer):boolean;
    function StackToPostFix:boolean;
    function ConvertInFixToPostFix:boolean;
    procedure procString;
    procedure procNumber;
    procedure procAttribute;
    procedure procEq;
    procedure procNe;
    procedure procGt;
    procedure procGe;
    procedure procLt;
    procedure procLe;
    procedure procAdd;
    procedure procSubtract;
    procedure procMultiply;
    procedure procDivide;
    procedure procAnd;
    procedure procOr;
    procedure procNot;
    procedure procLike;
    procedure procIn;
//  node functions
    procedure procName;
    procedure procPosition;
    procedure procFirst;
    procedure procLast;    
    procedure procPath;
    procedure procLocation;
    procedure procParentName;
    procedure procNameSpace;
    procedure procNameSpaceURI;
    procedure procBaseName;
    procedure procValue;
    procedure procChildCount;
    procedure procHasAttribute;
    procedure procHasChild;
// numerical functions
    procedure procSin;
    procedure procCos;
    procedure procSqr;
    procedure procSqrt;
    procedure procCeil;
    procedure procFloor;
    procedure procIsNumeric;
    procedure procIsDate;
// string functions
    procedure procUPPER;
    procedure procLOWER;
    procedure procTRIM;
    procedure procSoundex;
    procedure procLeft;
    procedure procRight;
    procedure procMid;
    procedure procsubstr_after;
    procedure procsubstr_before;
    procedure procReplace;
    procedure procLen;
    procedure procFix;
    procedure procFormat;
    procedure procYear;
    procedure procMonth;
    procedure procDay;
    procedure procDateAdd;
    procedure procEaster;
    procedure procWeekNumber;
    // conversion functions
    procedure procAsNumber;
    procedure procAsDate;
    procedure procParseFloat;
    // variable assignment and get
    procedure procAssign;
    procedure procGet;

    function CloseStackToPostFix: boolean;
    function OperatorsToPostFix(Level:integer): boolean;
    function FlushStackToPostFix: boolean;
    function runpop:variant;
    procedure runpush(value:variant);
    procedure SetonGetVariable(const Value: TVariableEvent);
    function IsLike(v1,v2:variant):boolean;
    procedure runOperator(op: TTokenOperator);
    procedure SetCurrentNode(const Value: TjanXMLNode2);
//    procedure GetElement(sender:Tobject;const VariableName:string;var VariableValue:variant;var handled:boolean);
    procedure GetAttribute(sender:Tobject;const VariableName:string;var VariableValue:variant;var handled:boolean);
    function IsIn(v1, v2: variant): boolean;
    procedure SetCurrentList(const Value: TList);
  public
    {Compiling expression evaluator for QXML expressions.}
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;
    {Frees any child objects}
    procedure getInFix(list:TStrings);
    {Fills list with parsed tokens}
    procedure getPostFix(list:TStrings);
    {Fills list with parsed tokens as obtained after infix to postfix conversion.}
    function Evaluate:variant;
    {Evaluates expression. Works very fast because the expression is semi-compiled.}
    procedure GetTokenList(list:TList;from,till:integer);
    {Allows evaluation of sub expressions. For future use.}
    property Expression:string read Fsource write Setsource;
    {Holds the QXML expression. Parses and compiles on write.}
    property CurrentNode:TjanXMLNode2 read FCurrentNode write SetCurrentNode;
    {Hold a reference to the current node. Node functions in the expression work with the current node.}
    property CurrentList:TList read FCurrentList write SetCurrentList;
    {The position of the current node within a nodeset beiing filtered. The first node having position=1}
    property onGetAttribute:TVariableEvent read FonGetVariable write SetonGetVariable;//event
    {Event is raised each time an attribute value is requested in the expression. In the present implementation

     this event is assigned to an internal method.}
  end;

  {$IFNDEF FPC}
  TjanXMLMenuItem2=class(TMenuItem)
  private
    FNode: TJanXMLNode2;
    procedure SetNode(const Value: TJanXMLNode2);
    public
      property Node:TJanXMLNode2 read FNode write SetNode;
  end;
  {$ENDIF}
  
  function HashThis(const s: string): cardinal;

implementation


const
  cr = chr(13)+chr(10);
  tab = chr(9);


  {** A basic hash function. This is pretty fast, and fairly good general
      purpose, but you may want to swap in a specialised version. }
  function HashThis(const s: string): cardinal;
  var
    h, g, i: cardinal;
  begin
    if (s = '') then
      raise EHashInvalidKeyError.Create('Key cannot be an empty string');
    h := $12345670;
    for i := 1 to Length(s) do begin
      h := (h shl 4) + ord(s[i]);
      g := h and $f0000000;
      if (g > 0) then
        h := h or (g shr 24) or g;
    end;
    result := h;
  end;



{ THash }

constructor THash.Create;
begin
  inherited Create;
  self.f_CurrentIterator.ck := -1;
  self.f_CurrentIterator.cx := 0;
  self.f_CurrentItemShift := c_HashInitialItemShift;
  self.FUpdateMasks;
  self.FUpdateBuckets;
  self.f_AllowCompact := true;
end;

procedure THash.Delete(const Key: string);
var
  k, x, i: integer;
begin
  { Hash has been modified, so disallow Next. }
  self.f_NextAllowed := false;
  if (self.FFindKey(Key, k, x)) then begin
    { Delete the Index entry. }
    i := self.f_Keys[k][x].ItemIndex;
    self.FDeleteIndex(i);
    { Add the index to the Spares list. }
    SetLength(self.f_SpareItems, Length(self.f_SpareItems) + 1);
    self.f_SpareItems[High(self.f_SpareItems)] := i;
    { Overwrite key with the last in the list. }
    self.f_Keys[k][x] := self.f_Keys[k][High(self.f_Keys[k])];
    { Delete the last in the list. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) - 1);
  end else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);

  self.FAutoCompact;
end;

function THash.Exists(const Key: string): boolean;
var
  dummy1, dummy2: integer;
begin
  result := FFindKey(Key, dummy1, dummy2);
end;

procedure THash.FSetOrAddKey(const Key: string; ItemIndex: integer);
var
  k, x, i: integer;
begin
  { Exists already? }
  if (self.FFindKey(Key, k, x)) then begin
    { Yep. Delete the old stuff and set the new value. }
    i := self.f_Keys[k][x].ItemIndex;
    self.FDeleteIndex(i);
    self.f_Keys[k][x].ItemIndex := ItemIndex;
    { Add the index to the spares list. }
    SetLength(self.f_SpareItems, Length(self.f_SpareItems) + 1);
    self.f_SpareItems[High(self.f_SpareItems)] := i;
  end else begin
    { No, create a new one. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) + 1);
    self.f_Keys[k][High(self.f_Keys[k])].Key := Key;
    self.f_Keys[k][High(self.f_Keys[k])].ItemIndex := ItemIndex;
    self.f_Keys[k][High(self.f_Keys[k])].Hash := HashThis(Key);
  end;
end;

function THash.FFindKey(const Key: string; var k, x: integer): boolean;
var
  i: integer;
  h: cardinal;
begin
  { Which bucket? }
  h := HashThis(Key);
  k := h and f_CurrentItemMask;
  result := false;
  { Look for it. }
  for i := 0 to High(self.f_Keys[k]) do
    if (self.f_Keys[k][i].Hash = h) or true then
      if (self.f_Keys[k][i].Key = Key) then begin
        { Found it! }
        result := true;
        x := i;
        break;
      end;
end;

procedure THash.Rename(const Key, NewName: string);
var
  k, x, i: integer;
begin
  { Hash has been modified, so disallow Next. }
  self.f_NextAllowed := false;
  if (self.FFindKey(Key, k, x)) then begin
    { Remember the ItemIndex. }
    i := self.f_Keys[k][x].ItemIndex;
    { Overwrite key with the last in the list. }
    self.f_Keys[k][x] := self.f_Keys[k][High(self.f_Keys[k])];
    { Delete the last in the list. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) - 1);
    { Create the new item. }
    self.FSetOrAddKey(NewName, i);
  end else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);
  self.FAutoCompact;
end;

function THash.CurrentKey: string;
begin
  if (not (self.f_NextAllowed)) then
    raise EHashIterateError.Create('Cannot find CurrentKey as the hash has '
      + 'been modified since Restart was called')
  else if (self.f_CurrentKey = '') then
    raise EHashIterateError.Create('Cannot find CurrentKey as Next has not yet '
      + 'been called after Restart')
  else
    result := self.f_CurrentKey;
end;

function THash.Next: boolean;
begin
  if (not (self.f_NextAllowed)) then
    raise EHashIterateError.Create('Cannot get Next as the hash has '
      + 'been modified since Restart was called');
  result := false;
  if (self.f_CurrentIterator.ck = -1) then begin
    self.f_CurrentIterator.ck := 0;
    self.f_CurrentIterator.cx := 0;
  end;
  while ((not result) and (self.f_CurrentIterator.ck <= f_CurrentItemMaxIdx)) do begin
    if (self.f_CurrentIterator.cx < Length(self.f_Keys[self.f_CurrentIterator.ck])) then begin
      result := true;
      self.f_CurrentKey := self.f_Keys[self.f_CurrentIterator.ck][self.f_CurrentIterator.cx].Key;
      inc(self.f_CurrentIterator.cx);
    end else begin
      inc(self.f_CurrentIterator.ck);
      self.f_CurrentIterator.cx := 0;
    end;
  end;
end;

procedure THash.Restart;
begin
  self.f_CurrentIterator.ck := -1;
  self.f_CurrentIterator.cx := 0;
  self.f_NextAllowed := true;
end;

function THash.FGetItemCount: integer;
var
  i: integer;
begin
  { Calculate our item count. }
  result := 0;
  for i := 0 to f_CurrentItemMaxIdx do
    inc(result, Length(self.f_Keys[i]));
end;

function THash.FAllocItemIndex: integer;
begin
  if (Length(self.f_SpareItems) > 0) then begin
    { Use the top SpareItem. }
    result := self.f_SpareItems[High(self.f_SpareItems)];
    SetLength(self.f_SpareItems, Length(self.f_SpareItems) - 1);
  end else begin
    result := self.FIndexMax + 1;
  end;
end;

procedure THash.Compact;
var
  aSpaces: array of boolean;
  aMapping: array of integer;
  i, j: integer;
begin
  { Find out where the gaps are. We could do this by sorting, but that's at
    least O(n log n), and sometimes O(n^2), so we'll go for the O(n) method,
    even though it involves multiple passes. Note that this is a lot faster
    than it looks. Disabling this saves about 3% in my benchmarks, but uses a
    lot more memory. }
  if (self.AllowCompact) then begin
    SetLength(aSpaces, self.FIndexMax + 1);
    SetLength(aMapping, self.FIndexMax + 1);
    for i := 0 to High(aSpaces) do
      aSpaces[i] := false;
    for i := 0 to High(aMapping) do
      aMapping[i] := i;
    for i := 0 to High(self.f_SpareItems) do
      aSpaces[self.f_SpareItems[i]] := true;

    { Starting at the low indexes, fill empty ones from the high indexes. }
    i := 0;
    j := self.FIndexMax;
    while (i < j) do begin
      if (aSpaces[i]) then begin
        while ((i < j) and (aSpaces[j])) do
          dec(j);
        if (i < j) then begin
          aSpaces[i] := false;
          aSpaces[j] := true;
          self.FMoveIndex(j, i);
          aMapping[j] := i
        end;
      end else
        inc(i);
    end;

    j := self.FIndexMax;
    while (aSpaces[j]) do
      dec(j);

    { Trim the items array down to size. }
    self.FTrimIndexes(j + 1);

    { Clear the spaces. }
    SetLength(self.f_SpareItems, 0);

    { Update our buckets. }
    for i := 0 to f_CurrentItemMaxIdx do
      for j := 0 to High(self.f_Keys[i]) do
        self.f_Keys[i][j].ItemIndex := aMapping[self.f_Keys[i][j].ItemIndex];
  end;
end;

procedure THash.FAutoCompact;
begin
  if (self.AllowCompact) then
    if (Length(self.f_SpareItems) >= c_HashCompactM) then
      if (self.FIndexMax * c_HashCompactR > Length(self.f_SpareItems)) then
        self.Compact;
end;

procedure THash.Clear;
var
  i: integer;
begin
  self.FClearItems;
  SetLength(self.f_SpareItems, 0);
  for i := 0 to f_CurrentItemMaxIdx do
    SetLength(self.f_Keys[i], 0);
end;

procedure THash.FUpdateMasks;
begin
  f_CurrentItemMask := (1 shl f_CurrentItemShift) - 1;
  f_CurrentItemMaxIdx := (1 shl f_CurrentItemShift) - 1;
  f_CurrentItemCount := (1 shl f_CurrentItemShift);
end;

procedure THash.FUpdateBuckets;
begin
  { This is just a temporary thing. }
  SetLength(self.f_Keys, self.f_CurrentItemCount);
end;

function THash.NewIterator: THashIterator;
begin
  result.ck := -1;
  result.cx := 0;
end;

function THash.Previous: boolean;
begin
  if (not (self.f_NextAllowed)) then
    raise EHashIterateError.Create('Cannot get Next as the hash has '
      + 'been modified since Restart was called');
  result := false;
  if (self.f_CurrentIterator.ck >= 0) then begin
    while ((not result) and (self.f_CurrentIterator.ck >= 0)) do begin
      dec(self.f_CurrentIterator.cx);
      if (self.f_CurrentIterator.cx >= 0) then begin
        result := true;
        self.f_CurrentKey := self.f_Keys[self.f_CurrentIterator.ck][self.f_CurrentIterator.cx].Key;
      end else begin
        dec(self.f_CurrentIterator.ck);
        if (self.f_CurrentIterator.ck >= 0) then
          self.f_CurrentIterator.cx := Length(self.f_Keys[self.f_CurrentIterator.ck]);
      end;
    end;
  end;
end;


{ TObjectHash }

procedure TObjectHash.FDeleteIndex(i: integer);
begin
  if FOwnsItems then
    self.f_Items[i].Free;
  self.f_Items[i] := nil;
end;

function TObjectHash.FGetItem(const Key: string): TObject;
var
  k, x: integer;
begin
  if (self.FFindKey(Key, k, x)) then
    result := self.f_Items[self.f_Keys[k][x].ItemIndex]
  else
    raise EHashFindError.CreateFmt('Key "%s" not found', [Key]);
end;

procedure TObjectHash.FMoveIndex(oldIndex, newIndex: integer);
begin
  self.f_Items[newIndex] := self.f_Items[oldIndex];
end;

procedure TObjectHash.FSetItem(const Key: string; Value: TObject);
var
  k, x, i: integer;
begin
  if (self.FFindKey(Key, k, x)) then begin
    if FOwnsItems then
      self.f_Items[self.f_Keys[k][x].ItemIndex].Free;
    self.f_Items[self.f_Keys[k][x].ItemIndex] := Value;
  end else begin
    { New index entry, or recycle an old one. }
    i := self.FAllocItemIndex;
    if (i > High(self.f_Items)) then
      SetLength(self.f_Items, i + 1);
    self.f_Items[i] := Value;
    { Add it to the hash. }
    SetLength(self.f_Keys[k], Length(self.f_Keys[k]) + 1);
    self.f_Keys[k][High(self.f_Keys[k])].Key := Key;
    self.f_Keys[k][High(self.f_Keys[k])].ItemIndex := i;
    self.f_Keys[k][High(self.f_Keys[k])].Hash := HashThis(Key);
    { Hash has been modified, so disallow Next. }
    self.f_NextAllowed := false;
  end;
end;

function TObjectHash.FIndexMax: integer;
begin
  result := High(self.f_Items);
end;

procedure TObjectHash.FTrimIndexes(count: integer);
begin
  SetLength(self.f_Items, count);
end;

procedure TObjectHash.FClearItems;
var
  i: integer;
begin
  if FOwnsItems then
    for i := 0 to High(self.f_Items) do
      if (Assigned(self.f_Items[i])) then
        self.f_Items[i].Free;
  SetLength(self.f_Items, 0);
end;

destructor TObjectHash.Destroy;
var
  i: integer;
begin
  FClearItems;
  inherited;
end;

procedure TObjectHash.SetOwnsItems(const Value: boolean);
begin
  FOwnsItems := Value;
end;


// xpath operators

// check the element name
function xpElement(node:TjanXMLNode2;aname,avalue:string):boolean;
begin
  result:=node.name=aname;
end;

// check the attribute existence
function xpNop(node:TjanXMLNode2;aname,avalue:string):boolean;
begin
  result:=false;
end;


function xpAttribute(node:TjanXMLNode2;aname,avalue:string):boolean;
begin
  result:=node.indexOfAttribute(aname)<>-1;
end;

// check the attribute=value
function xpAttributeEQ(node:TjanXMLNode2;aname,avalue:string):boolean;
begin
  result:=node.indexOfAttribute(aname)<>-1;
  if result then
    result:=node.attribute[aname]=avalue;
end;

// check the attribute<>value
function xpAttributeNE(node:TjanXMLNode2;aname,avalue:string):boolean;
begin
  result:=node.indexOfAttribute(aname)<>-1;
  if result then
    result:=node.attribute[aname]<>avalue;
end;

// check the presence of a named child
function xpChild(node:TjanXMLNode2;aname,avalue:string):boolean;
var
  i,c:integer;
  n:TjanXMLNode2;
begin
  result:=false;
  c:=node.nodes.Count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    n:=TjanXMLNode2(node.nodes[i]);
    if n.name=aname then
      result:=true;
      exit;
  end;
end;



// check the presence of a named child with value
function xpChildEQ(node:TjanXMLNode2;aname,avalue:string):boolean;
var
  i,c:integer;
  n:TjanXMLNode2;
begin
  result:=false;
  c:=node.nodes.Count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    n:=TjanXMLNode2(node.nodes[i]);
    if n.name=aname then
      if n.text=avalue then begin
        result:=true;
        exit;
      end;
  end;
end;

// check the presence of a named child with<>value
function xpChildNE(node:TjanXMLNode2;aname,avalue:string):boolean;
var
  i,c:integer;
  n:TjanXMLNode2;
begin
  result:=false;
  c:=node.nodes.Count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    n:=TjanXMLNode2(node.nodes[i]);
    if n.name=aname then
      if n.text<>avalue then begin
        result:=true;
        exit;
      end;
  end;
end;

{ TjanXMLNode2 }

procedure TjanXMLNode2.addNode(node: TjanXMLNode2);
begin
  FNodes.Add(node);
  node.parentNode:=self;
  if (self is TjanXMLParser2) then
    node.FParser:=(self as TjanXMLParser2)
  else
    node.FParser:=self.FParser;
  if node.FParser<>nil then begin
    if not node.FParser.IsParsing then begin
      if node.FParser.AutoIndexByNameAndId then begin
        node.FParser.IndexNodeByNameAndId(node);
      end;
    end;
  end;
end;

function TjanXMLNode2.cloneNode: TjanXMLNode2;
var i:integer;
    n:TjanXMLNode2;
    a:TjanXMLAttribute2;
begin
  result:=TjanXMLNode2.create;
  result.name:=name;
  result.text:=text;
  if Fattributes.Count>0 then
    for i:=0 to Fattributes.Count-1 do begin
      a:=TjanXMLAttribute2(attributes[i]).cloneAttribute;
      result.attributes.Add(a);
    end;
  if nodes.count>0 then begin
    for i:=0 to nodes.count-1 do begin
      n:=TjanXMLNode2(nodes[i]).cloneNode;
      result.Nodes.Add(n);
      n.ParentNode := Result;
    end;
  end;
end;

constructor TjanXMLNode2.create;
begin
  inherited;
  Fnodes:=TjanXMLNodeList2.create;
  Fattributes:=TjanXMLNodeList2.create;
  FCDATA:=false;
end;

function TjanXMLNode2.deleteAttribute(attribute:TjanXMLAttribute2): boolean;
var
  index:integer;
begin
  result:=false;
  index:=FAttributes.IndexOf(attribute);
  if index=-1 then exit;
  FAttributes.Delete(index);
  attribute.Free;
  result:=true;
end;

procedure TjanXMLNode2.deleteNode(node: TjanXMLNode2);
var
  i:integer;
  dn:TjanXMLNode2;
begin
  i:=Fnodes.IndexOf(node);
  dn:=TjanXMLNode2(Fnodes[i]);
  dn.free;
  Fnodes.Delete(i);
end;

destructor TjanXMLNode2.destroy;
begin
  // try this
  if FParser<>nil then
    if not FParser.IsDestroying then begin
      if FParser.AutoIndexByNameAndId then begin
        FParser.RemoveNodeFromIndexByNameAndId(self);
      end;
    end;
  Fattributes.free;
  if FParser<>nil then
    if Not FParser.IsDestroying then
      RemoveNodeIndexes;
  FNodes.free;
  inherited;

end;

{function TjanXMLNode2.processXSL(node: TjanXMLNode2): string;
begin
  if name='xsl:apply-templates' then
    result:=xsl_apply_templates(node)
  else
    result:='';
end;

function TjanXMLNode2.xsl_apply_templates(node: TjanXMLNode2): string;
var
  pattern:TjanXPathParser2;
  n,matchnode:TjanXMLNode2;
  nodelist:Tlist;
  i,c:integer;
begin
  pattern:=TjanXPathParser2.Create;
  pattern.pattern:=attribute['select'];
  if pattern.pattern='' then
    result:=node.execXSL(node)
  else begin
    n:=self.Fparser.selectTemplate(pattern);
    if n=nil then
      result:=''
    else begin
      nodelist:=Tlist.Create;
      try
     //   node.selectNodes(nodelist,pattern);
        result:='';
        c:=nodelist.count;
        if c<>0 then
          for i:=0 to c-1 do begin
            matchnode:=TjanXMLNode2(nodelist[i]);
            result:=result+n.execXSL(matchnode);
          end;
      finally
        nodelist.free;
      end;
    end;
  end;
  pattern.free;
end;

function TjanXMLNode2.match(pattern:string):boolean;
var
  s:string;
begin
  s:=pattern;
  result:=self.name=pattern;
end;}

procedure TjanXMLNode2.selectNodes(nodelist:TList;pattern:string;single:boolean=false);
var
  i,c:integer;
  xpp:TjanXPathParser2;
begin
  xpp:=TjanXPathParser2.Create;
  try
    xpp.pattern:=pattern;
    xpp.selectNodes(self,nodelist,single);
  finally
    xpp.free;
  end;
end;


{function TjanXMLNode2.selectTemplate(pattern:TjanXPathParser2):TjanXMLNode2;
var
  n:TjanXMLNode2;
  i,c:integer;
  ename,amatch:string;
begin
  result:=self;
  if (self.name='xsl:template') and (attribute['match']=pattern.pattern) then exit; // got it
  result:=nil;
  c:=self.nodes.Count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    result:=TjanXMLNode2(self.nodes[i]).selectTemplate(pattern);
    if result<>nil then exit;
  end;
end;

function TjanXMLNode2.execXSL(node: TjanXMLNode2): string;
var
  i,c:integer;
  aname:string;
begin
  if pos('xsl:',self.name)>0 then begin
    result:=processXSL(node);
    exit;
  end;
  result:='<'+self.name;
  c:=self.attributecount;
  if c<>0 then
    for i:=0 to c-1 do begin
      aname:=self.attributename[i];
      result:=result+' '+aname+'='''+self.attribute[aname]+'''';
    end;
  if self.text<>'' then begin
    result:=result+'>'+self.text;
  end;
  c:=self.nodes.count;
  if c=0 then begin
    if self.text='' then
      result:=result+'/>'+cr
    else
      result:=result+'</'+self.name+'>'+cr;
  end
  else begin
    if self.text='' then
      result:=result+'>';
    for i:=0 to c-1 do begin
      result:=result+TjanXMLNode2(self.nodes[i]).execXSL(node);
    end;
    result:=result+'</'+self.name+'>';
  end;
end;}

function TjanXMLNode2.getAttribute(index: variant): variant;
var
  i:integer;
begin
  result:='';
  case vartype(index) of
    varstring:
      begin
        i:=indexOfAttribute(index);
        if i<>-1 then
          result:=TjanXMLAttribute2(Fattributes[i]).Value;
      end;
    varinteger:
      begin
        i:=index;
        if index<Fattributes.Count then
          result:= TjanXMLAttribute2(Fattributes[i]).Value;
      end;
  end
end;

function TjanXMLNode2.getAttributeCount: integer;
begin
  result:=Fattributes.Count;
end;

function TjanXMLNode2.Getattributename(index: integer): string;
begin
  if index<Fattributes.count then
    result:=TjanXMLAttribute2(Fattributes[index]).name
  else
    result:='';
end;

function TjanXMLNode2.moveto(node: TjanXMLNode2):boolean;
var
  n:TjanXMLNode2;
  index:integer;
begin
  result:=false;
  n:=self.parentNode;
  if n=nil then exit;
  index:=n.nodes.IndexOf(self);
  if index=-1 then exit;
  n.nodes.Delete(index);
  node.addNode(self);
  result:=true;
end;

function TjanXMLNode2.renameAttribute(oldname, newname: string): boolean;
var
  index,p:integer;
  s:string;
begin
  result:=false;
  index:=indexOfAttribute(oldname);
  if index=-1 then exit;
  TjanXMLAttribute2(FAttributes[index]).name:=newname;
  result:=true;
end;

procedure TjanXMLNode2.setAttribute(index: variant; const Value: variant);
var
  idx:integer;
  a:TjanXMLAttribute2;
begin
  case vartype(index) of
  varstring:
    begin
      idx:=indexofAttribute(index);
      if idx=-1 then begin
        a:=TjanXMLAttribute2.Create;
        a.name:=index;
        a.value:=value;
        FAttributes.Add(a);
      end
      else
        TjanXMLAttribute2(Fattributes[idx]).value:=value;
    end;
  varinteger:
    begin
      idx:=index;
      if idx<FAttributes.count then
        TjanXMLAttribute2(Fattributes[idx]).value:=value;
    end;
  end;
end;


procedure TjanXMLNode2.Setname(const Value: string);
var
  p:integer;
begin
  p:=pos(':',value);
  if p=0 then
    Fname := Value
  else begin
    Fnamespace:=copy(value,1,p-1);
    Fname:=value;
  end;
end;


procedure TjanXMLNode2.SetParentNode(const Value: TjanXMLNode2);
begin
  FParentNode := Value;
end;

procedure TjanXMLNode2.Settext(const Value: string);
begin
  FCDATA:= (posstr('<',value)<>0) or (posstr('>',value)<>0);
  Ftext := Value;
end;


function TjanXMLNode2.indexOfAttribute(aname: string): integer;
var
  i,c:integer;
begin
  result:=-1;
  c:=attributes.Count;
  if c=0 then exit;
  for i:=0 to c-1 do
    if TjanXMLAttribute2(FAttributes[i]).name=aname then begin
      result:=i;
      exit;
    end;
end;

function TjanXMLNode2.hasAttribute(aname: string): boolean;
begin
  result:=self.indexOfAttribute(aname)<>-1;
end;

procedure TjanXMLNode2.listChildren(alist: TList);
var
  i,c:integer;
  n:TjanXMLNode2;
begin
  c:=nodes.Count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    n:=TjanXMLNode2(nodes[i]);
    alist.Add(n);
    n.listChildren(alist);
  end;
end;

function TjanXMLNode2.getChildByID(aid: string;descent:boolean=false): TjanXMLNode2;
var
  i,c:integer;
  n:TjanXMLNode2;
begin
  result:=nil;
  c:=self.nodes.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    n:=TjanXMLNode2(nodes[i]);
    if n.attribute['id']=aid then begin
      result:=n;
      break;
    end
    else begin
      if not descent then
        continue
      else begin
        result:=n.getChildByID(aid,descent);
        if result<>nil then
          break
        else
          continue;
      end;
    end;
  end;
end;

function TjanXMLNode2.getChildByName(aname: string;descent:boolean=false): TjanXMLNode2;
var
  i,c:integer;
  n:TjanXMLNode2;
begin
  result:=nil;
  c:=self.nodes.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    n:=TjanXMLNode2(nodes[i]);
    if n.name=aname then begin
      result:=n;
      exit;
    end
    else begin
      if not descent then
        continue
      else begin
        result:=n.getChildByName(aname,descent);
        if result<>nil then
          exit
        else
          continue;
      end;
    end;
  end;
end;




function TjanXMLNode2.transformNode(stylesheet: TjanXMLparser2): string;
begin
  result:=stylesheet.transformNode_(self);
end;

procedure TjanXMLNode2.selectNodes(parser: TjanXMLParser2; nodelist: TList;
  pattern: string; single: boolean);
var
  i,c,p,pfs,ppred:integer;
  mi,mc:integer;
  xpp:TjanXPathParser2;
  xplocation,xpstep,predicates,predicate:string;
  axis,nodetest:string;
  ncontext:TjanXMLNode2;
  firstStep, decend:boolean;
  mlist:TList;
begin
  xplocation:=pattern;
  ncontext:=self;
  firstStep:=true;
  while xplocation<>'' do begin
    if xplocation[1]='/' then begin
      decend:=true;
      delete(xplocation,1,1);
    end
    else
      decend:=false;
    pfs:=pos('/',xplocation);
    if pfs>0 then begin
      xpstep:=copy(xplocation,1,pfs-1);
      delete(xplocation,1,pfs);
    end
    else begin
      xpstep:=xplocation;
      xplocation:='';
    end;
    ppred:=pos('[',xpstep);
    if ppred>0 then begin
      predicates:=copy(xpstep,ppred,maxint);
      if predicates[length(predicates)]<>']' then
        raise exception.create('Missing ] in pattern '+pattern);
      xpstep:=copy(xpstep,1,ppred-1);
    end
    else begin
      predicates:='';
    end;
    p:=pos('::',xpstep);
    if p>0 then begin
      axis:=copy(xpstep,1,p-1);
      nodetest:=copy(xpstep,p+2,maxint);
    end
    else if xpstep='.' then begin
      axis:='self';
      nodetest:='';
    end
    else if xpstep='..' then begin
      axis:='parent';
      nodetest:='';
    end
    else begin
      axis:='child';
      nodetest:=xpstep;
    end;
    if (axis='parent') then begin
      if (ncontext.parentNode=nil ) then
        raise exception.create('Can not locate parent node in location '+pattern);
      ncontext:=ncontext.parentNode;
      if nodetest='' then
        nodelist.add(ncontext)
      else
        axis:='child';
    end;
    if axis='self' then begin
      nodelist.add(ncontext);
    end
    else if axis='child' then begin
      if firstStep then begin
        firstStep:=false;
        ncontext.getChildrenByName(nodetest,nodelist,true);
      end
      else begin
        mc:=nodelist.count;
        try
          mlist:=TList.create;
          for i:=0 to mc-1 do
            mlist.add(nodelist[i]);
          nodelist.clear;
          for i:=0 to mc-1 do begin
            ncontext:=TjanXMLNode2(mlist[i]);
            ncontext.getChildrenByName(nodetest,nodelist,decend);
          end;
        finally
         mlist.free;
        end;
      end;
    end;
    mc:=nodelist.count;
    if mc=0 then exit;
    while predicates<>'' do begin
      if predicates[1]<>'[' then
        raise exception.create('Expected [ in predicates '+predicates+' of pattern '+pattern);
      ppred:=pos(']',predicates);
      if ppred=0 then
        raise exception.create('Expected ] in predicates '+predicates+' of pattern '+pattern);
      predicate:=copy(predicates,2,ppred-2);
      delete(predicates,1,ppred);
      if predicate<>'' then begin
        xpp:=parser.getXPathParser(predicate);
        xpp.filterNodes(nodelist);
      end;
      if nodelist.count=0 then break;
    end;
  end;
//  xpp:=parser.getXPathParser(pattern);
//  xpp.selectNodes(self,nodelist,single);
end;

function TjanXMLNode2.namespaceURI: string;
var
  n:TjanXMLNode2;
  ns,nsURI,xmlns:string;
begin
  result:='';
  n:=self;
  ns:=n.namespace;
  if ns='' then exit;
  ns:='xmlns:'+ns;
  result:=n.attribute[ns];
  if result<>'' then exit;
  while n.parentNode<>nil do begin
    n:=n.parentnode;
    result:=n.attribute[ns];
    if result<>'' then exit;
  end;
end;

procedure TjanXMLNode2.insertNode(index: integer; node: TjanXMLNode2);
begin
  FNodes.Insert(index,node);
end;

function TjanXMLNode2.forceChildByName(aname: string): TjanXMLNode2;
begin
  result:=getChildByName(aname);
  if result<>nil then exit;
  result:=TjanXMLNode2.create;
  result.name:=aname;
  AddNode(result);
end;

function TjanXMLNode2.GetID: string;
begin
  result:=attribute['id'];
end;

procedure TjanXMLNode2.SetID(const Value: string);
begin
  attribute['id']:=value;
end;

function TjanXMLNode2.getChildByAttribute(AttributeName,
  AttributeValue: string): TjanXMLNode2;
var
  i,c:integer;
  n:TjanXMLNode2;
begin
  result:=nil;
  c:=self.nodes.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    n:=TjanXMLNode2(nodes[i]);
    if n.attribute[AttributeName]<>AttributeValue then
      continue;
    result:=n;
    exit;
  end;
end;

function TjanXMLNode2.addChildByName(aname: string): TjanXMLNode2;
begin
  result:=TjanXMLNode2.create;
  result.name:=aname;
  addNode(result);
end;

function TjanXMLNode2.getChildByPath(apath: string): TjanXMLNode2;
var
  tmp, atom:string;
  p:integer;
  n:TjanXMLNode2;
begin
  result:=nil;
  tmp:=apath;
  p:=pos('/',tmp);
  if p=0 then begin
    atom:=tmp;
    result:=getChildByName(atom);
  end
  else begin
    n:=self;
    atom:=copy(tmp,1,p-1);
    delete(tmp,1,p);
    while atom<>'' do begin
      result:=n.getChildbyName(atom);
      if result=nil then exit;
      n:=result;
      p:=pos('/',tmp);
      if p=0 then begin
        atom:=tmp;
        tmp:='';
      end
      else begin
        atom:=copy(tmp,1,p-1);
        delete(tmp,1,p);
      end;
    end;
  end;
end;

function TjanXMLNode2.getChildText(pName: string): string;
var
  xc:TjanXMLNode2;
begin
  result:='';
  xc:=getchildbyName(pName);
  if xc=nil then exit;
  result:=xc.text;
end;

function TjanXMLNode2.forceChildByPath(apath: string): TjanXMLNode2;
var
  tmp, atom:string;
  p:integer;
  n:TjanXMLNode2;
begin
  result:=nil;
  tmp:=apath;
  p:=pos('/',tmp);
  if p=0 then begin
    atom:=tmp;
    result:=forceChildByName(atom);
  end
  else begin
    n:=self;
    atom:=copy(tmp,1,p-1);
    delete(tmp,1,p);
    while atom<>'' do begin
      result:=n.forceChildbyName(atom);
      n:=result;
      p:=pos('/',tmp);
      if p=0 then begin
        atom:=tmp;
        tmp:='';
      end
      else begin
        atom:=copy(tmp,1,p-1);
        delete(tmp,1,p);
      end;
    end;
  end;
end;

function TjanXMLNode2.getParentByName(aname: string): TjanXMLNode2;
var
  n:TjanXMLNode2;
begin
  result:=nil;
  if parentNode=nil then exit;
  n:=parentNode;
  while n<>nil do begin
    if n.name=aname then begin
      result:=n;
      exit;
    end;
    n:=n.parentnode;
  end;
end;

function TjanXMLNode2.executeNode(parser: TjanXMLParser2): boolean;
var
  script:string;
  i,c:integer;
  xpp:TjanXPathParser2;
  atom:string;
  act:string;
  xcall,xswitch:TjanXMLNode2;
  runChildNodes:boolean;
  t1,t2:integer;
  skip:boolean;

  function getFunctionNode:TjanXMLNode2;
  begin
    result:=self.getParentByName('xel:program');
    if result=nil then exit;
    result:=result.getChildByID(text);
    if result=nil then exit;
    if result.name<>'xel:function' then result:=nil;
  end;
begin
  result:=false;
  atom:=name;
  if pos('xel:',atom)=1 then begin
    act:=self['act'];
    xpp:=parser.getXPathParser(act);
    delete(atom,1,4);
    if atom='program' then skip:=true
    else if atom='function' then skip:=true
    else if atom='var' then skip:=true
    else if atom='do' then begin
      xpp.testNode(self);
      exit;
    end
    else if atom='call' then begin
      xcall:=GetFunctionNode;
      if xcall=nil then exit;
      c:=xcall.nodes.count;
      if c=0 then exit;
      for i:=0 to c-1 do
         TjanXMLNode2(xcall.Nodes[i]).executeNode(parser);
      exit;
    end
    else if atom='if' then begin
      if not xpp.testNode(self) then exit;
      c:=nodes.count;
      if c=0 then exit;
      for i:=0 to c-1 do
         TjanXMLNode2(self.Nodes[i]).executeNode(parser);
      exit;
    end
    else if atom='while' then begin
      t1:=gettickcount;
      while xpp.testNode(self) do begin
        t2:=gettickcount;
        if (t2-t1)>10000 then exit;
        c:=nodes.Count;
        if c=0 then exit;
        for i:=0 to c-1 do
          TjanXMLNode2(Nodes[i]).executeNode(parser);
      end;
      exit;
    end
    else if atom='switch' then begin
      result:=xelSwitch(self,parser);
      exit;
    end
    else if atom='custom' then begin
      if assigned(parser.onXEL) then
        parser.onXEL(parser,self,act);
      exit;  
    end;
  end;
  c:=nodes.Count;
  if c<>0 then
    for i:=0 to c-1 do
      TjanXMLNode2(Nodes[i]).executeNode(parser);
end;

function TjanXMLNode2.xelSwitch(anode: TjanXMLNode2;
  parser: TjanXMLParser2): boolean;
var
  xpp:TjanXPathParser2;
  xcase:TjanXMLNode2;
  i,c:integer;
  ii,cc:integer;
begin
  result:=false;
  c:=anode.nodes.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xcase:=TjanXMLNode2(anode.nodes[i]);
    if xcase.name<>'xel:case' then continue;
    xpp:=parser.getXPathParser(xcase['act']);
    if not xpp.testNode(xcase) then  continue;
    cc:=xcase.nodes.count;
    if cc=0 then exit;
    for ii:=0 to cc-1 do
      TjanXMLNode2(xcase.nodes[ii]).executeNode(parser);
    exit;
  end;
end;

function TjanXMLNode2.getXELVariable(aname: string): TjanXMLNode2;
var
  n, nscope:TjanXMLNode2;
  stopsearch:boolean;
begin
// lookup first matching variable
  result:=nil;
  stopsearch:=false;
  nscope:=self;
  repeat
    n:=nscope.getParentByName('xel:function');
    if n=nil then begin
      n:=nscope.getParentByName('xel:program');
      if n=nil then exit;
      stopsearch:=true;
    end;
    nscope:=n;
    n:=n.getChildByID(aname);
    if n<>nil then
      if n.name='xel:var' then begin
        result:=n;
        exit;
      end;
  until stopsearch;
end;

function TjanXMLNode2.FirstChild: TjanXMLNode2;
begin
  result:=nil;
  if nodes.Count=0 then exit;
  result:=TjanXMLNode2(self.nodes[0]);
end;

function TjanXMLNode2.LastChild: TjanXMLNode2;
var
  c:integer;
begin
  result:=nil;
  c:=nodes.Count;
  if c=0 then exit;
  result:=TjanXMLNode2(self.nodes[c-1]);
end;

function TjanXMLNode2.NextSibling: TjanXMLNode2;
var
  xn:TjanXMLNode2;
  i,c:integer;
begin
  result:=nil;
  if self.parentNode=nil then exit;
  xn:=self.parentNode;
  i:=xn.nodes.IndexOf(self);
  if i<0 then exit;
  c:=xn.nodes.count;
  if i<(c-1) then
    result:=TjanXMLNode2(xn.nodes[i+1]);
end;

function TjanXMLNode2.PreviousSibling: TjanXMLNode2;
var
  xn:TjanXMLNode2;
  i,c:integer;
begin
  result:=nil;
  if self.parentNode=nil then exit;
  xn:=self.parentNode;
  i:=xn.nodes.IndexOf(self);
  if i<1 then exit;
  result:=TjanXMLNode2(xn.nodes[i-1]);
end;

function TjanXMLNode2.childCount: integer;
begin
  result:=nodes.count;
end;

function TjanXMLNode2.GetchildNode(index: integer): TjanXMLNode2;
begin
  result:=nil;
  if index>=nodes.count then exit;
  result:=TjanXMLNode2(nodes[index]);
end;



function TjanXMLNode2.deleteAttribute(attributeName: string): boolean;
var
  index:integer;
  att:TjanXMLAttribute2;
begin
  result:=false;
  index:=self.indexOfAttribute(attributeName);
  if index=-1 then exit;
  att:=TjanXMLAttribute2(self.attributes[index]);
  self.attributes.Delete(index);
  att.free;
  result:=true;
end;


function TjanXMLNode2.NodeIndex: integer;
begin
  if parentNode=nil then
    result:=-1
  else
    result:=parentNode.nodes.IndexOf(self);  
end;

function TjanXMLNode2.GetRootNode: TjanXMLNode2;
begin
  result:=self;
  while result.parentNode<>nil do
   result:=result.parentNode;
end;

procedure TjanXMLNode2.getChildrenByName(aname: string; nodelist: TList;
  descent: boolean);
var
  i,c:integer;
  n:TjanXMLNode2;
begin
  c:=self.nodes.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    n:=TjanXMLNode2(nodes[i]);
    if n.name=aname then begin
      nodelist.Add(n);
    end
    else begin
      if not descent then
        continue
      else
        n.getChildrenByName(aname,nodelist,descent);
    end;
  end;
end;

procedure TjanXMLNode2.getHtmlOptions(list: Tstringlist; path: string);
var
  i,c:integer;
  xn:TjanXMLNode2;
begin
  c:=childcount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xn:=childnode[i];
    list.Add('<option value="'+xn['id']+'">'+path+xn.text+'</option>');
    xn.getHtmlOptions(list,path+path);
  end;
end;

function TjanXMLNode2.namedChildCount(pname: string): integer;
var
  i,c:integer;
begin
  result:=0;
  c:=nodes.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    if childnode[i].name=pname then
      inc(result);
  end;
end;


procedure TjanXMLNode2.RemoveNodeIndexes;
var
  i,c:integer;
  xn:TjanXMLNode2;
begin
  if FParser<>nil then begin
    if FParser.AutoIndexByNameAndId then begin
      c:=FNodes.Count;
      if c>0 then
        for i:=0 to c-1 do begin
          xn:=TjanXMLNode2(FNodes[i]);
          FParser.RemoveNodeFromIndexByNameAndId(xn);
        end;
    end
  end;
end;

function TjanXMLNode2.getAsFloat(index: variant): extended;
begin
  try
    result:=attribute[index];
  except
    result:=0;
  end;
end;


function TjanXMLNode2.getAsInteger(index: variant): integer;
begin
  try
    result:=attribute[index];
  except
    result:=0;
  end;
end;


function TjanXMLNode2.getAsBoolean(index: variant): boolean;
var
  tmp:variant;
begin
  tmp:=attribute[index];
  if tmp=null then
    result:=false
  else begin
    case VarType(tmp) of
      varString: result:=(tmp<>'') and (tmp<>'0');
      varBoolean: result:=tmp;
    else
      result:=tmp;
    end;
  end;
end;


function TjanXMLNode2.getAsDateTime(index: variant): TDateTime;
begin
  try
    result:=attribute[index];
  except
    result:=0;
  end;
end;


function TjanXMLNode2.getAsText(index: variant): string;
var
  tmp:variant;
begin
  tmp:=attribute[index];
  if tmp=null then
    result:=''
  else
    result:=tmp;    
end;

procedure TjanXMLNode2.AddParsedAttribute(pName, pValue: string);
var
  idx:integer;
  a:TjanXMLAttribute2;
begin
  a:=TjanXMLAttribute2.Create;
  a.Name:=pName;
  a.Value:=pValue;
  FAttributes.Add(a);
end;

{ TjanXMLNodeList2 }

procedure TjanXMLNodeList2.Clear;
var
  i,c:integer;
begin
  c:=self.Count;
  if c>0 then
    for i:=0 to c-1 do
      TjanXMLNodeList2(self.Items[i]).free;
  inherited;
end;

destructor TjanXMLNodeList2.destroy;
var
  i,c:integer;
begin
  clear;
  inherited;
end;

{ TjanXMLParser2 }


{ TjanXMLParser2 }

function TjanXMLParser2.AsText: string;
var
  version,encoding:string;
begin
  FOutPut:='';
  FXMLSize:=$10000;
  FXMLPosition:=0;
  SetLength(FOutPut,FXMLSize);
  FXMLP:=pointer(FOutPut);
  FOutputdepth:=0;
  version:=declaration.attribute['version'];
  if version='' then
    version:='1.0';
  encoding:=declaration.attribute['encoding'];
  if encoding='' then
    encoding:='ISO-8859-1';
  XMLPut('<?xml version="'+version+'" encoding="'+encoding+'" ?>'+cr);
  OutputNode(self);
  result:=copy(FOutput,1,FXMLPosition);
  FOutput:='';
end;

function TjanXMLParser2.cdatacheck(value: string): string;
begin
// check if value containts < or >, if so output
// <![CDATA[   ... ]]>
  result:=value;
  if (posstr('<',value)=0) and (posstr('>',value)=0) then exit;
  result:='<![CDATA['+value+']]>';
end;

constructor TjanXMLParser2.create;
begin
  inherited;
  FNodeIndex:=TObjectHash.create;
  FIndexesByNameAndId:=TObjectHash.create;
  FIndexesByNameAndId.OwnsItems:=true;
  FPageSize:=$10000;
  FDeclaration:=TjanXMLNode2.create;
  FPatterns:=TjanXPathParserList2.create;
end;

destructor TjanXMLParser2.destroy;
begin
  FIsDestroying:=true;
  FNodeIndex.Free;
  FIndexesByNameAndId.Free;
  FDeclaration.free;
  FPatterns.free;
  inherited;

end;

procedure TjanXMLParser2.ExecXSLT(node,context: TjanXMLNode2; matchlist,
  templatelist: TList);
begin
  if node.name='xsl:value-of' then begin
    xsl_value_of(node,context, matchlist, templatelist)
  end
  else if node.name='xsl:call-template' then begin
    xsl_call_template(node,context, matchlist, templatelist)
  end
  else if node.name='xsl:apply-templates' then begin
    xsl_apply_templates(node,context, matchlist, templatelist)
  end
  else if node.name='xsl:if' then begin
    xsl_if(node,context, matchlist, templatelist)
  end
  else if node.name='xsl:attribute' then begin
    xsl_attribute(node,context, matchlist, templatelist);
  end
  else if node.name='xsl:element' then begin
    xsl_element(node,context, matchlist, templatelist);
  end
  else if node.name='xsl:choose' then begin
    xsl_choose(node,context, matchlist, templatelist);
  end
  else if node.name='xsl:for-each' then begin
    xsl_for_each(node,context, matchlist, templatelist);
  end
  else if node.name='xsl:comment' then begin
    xsl_comment(node,context, matchlist, templatelist);
  end
end;

function TjanXMLParser2.getXML: string;
begin
  result:=AsText;
end;

function TjanXMLParser2.getXPathParser(pattern: string): TjanXPathParser2;
var
  index:integer;
  xpp:TjanXPathParser2;
begin
  index:=FPatterns.IndexOf(pattern);
  if index<>-1 then
    result:=TjanXPathParser2(FPatterns.objects[index])
  else begin
    xpp:=TjanXPathParser2.Create;
    xpp.pattern:=pattern;
    result:=xpp;
    FPatterns.AddObject(pattern,xpp);
  end;
end;

procedure TjanXMLParser2.LoadXML(filename: string);
begin
  Fxml:=loadstring(filename);
  FParseError:=parse;
end;

procedure TjanXMLParser2.OutputNode(node: TjanXMLNode2);
var
 i,c:integer;
 att,attname,attvalue:string;
 spc:string;
begin
  if FoutputDepth=0 then
    spc:=''
  else
    spc:=stringofchar(' ',2*FoutputDepth);
  XMLPut(spc+'<'+node.name);
  c:=node.FAttributes.Count;
  if c<>0 then begin
    att:='';
    for i:=0 to c-1 do begin
      attname:=TjanXMLAttribute2(node.FAttributes[i]).name;
      attvalue:=TjanXMLAttribute2(node.FAttributes[i]).value;
      // JV-20040519
      attvalue:=q_replacestr(attvalue,'<','&lt;');
      attvalue:=q_replacestr(attvalue,'>','&gt;');
      attvalue:=q_replacestr(attvalue,'&','&amp;');
      attvalue:=q_replacestr(attvalue,'''','&apos;');
      attvalue:=q_replacestr(attvalue,'"','&quot;');
      att:=att+' '+attname+'="'+attvalue+'"'
    end;
    XMLPut(att);
  end;
  c:=node.FNodes.Count;
  if (c=0) and (node.text='') then begin
    XMLPut('/>');
    exit;
  end;
  if c=0 then begin // version 3.01
    if node.CDATA then
      XMLPut('><![CDATA['+node.text+']]></'+node.name+'>')
    else
     // XMLPut('>'+cdatacheck(node.text)+'</'+node.name+'>');
      XMLPut('>'+node.text+'</'+node.name+'>');
    exit;
  end;
  if node.CDATA then // version 3.01
    XMLPut('><![CDATA['+node.text+']]>')
  else
    XMLPut('>'+node.text);
//  XMLPut('>'+cdatacheck(node.text));
  inc(FOutputDepth);
  for i:=0 to c-1 do begin
    XMLPut(cr);
    OutputNode(TjanXMLNode2(node.Fnodes.items[i]));
  end;
  XMLPut(cr+spc+'</'+node.name+'>');
  dec(FOutputDepth);
end;

function TjanXMLParser2.parse:string;
var
  src:string;
  p,p2:integer;
  atom:string;
begin
  // here it all happens
  FIndexesByNameAndId.Clear;
  FAttributes.Clear;
  FIsDestroying:=true;
  Fnodes.Clear;
  FIsDestroying:=false;
  FPatterns.Clear;
  Fname:='';
  ftext:='';
  Fscan:=1;
  FIsParsing:=true;
  FparseRoot:=self;
  // skip any <?xml version='1.0'?>
  p:=posstr('<',Fxml);
  if p>0 then
    if copy(Fxml,p,5)='<?xml' then begin
      p2:=posstr('?>',Fxml,p);
      if p2=0 then begin
        result:='Missing ?> in xml declaration.';
        exit;
      end
      else begin
        FScan:=p2+2;
        atom:=trim(copy(Fxml,p+6,p2-p-6));
        parseattributes(FDeclaration,atom);
      end;
    end;
  try
    parseNode(nil,nil);
    result:='';
  except
    on E: exception do begin
      if Fscan<21 then
        p:=1
      else
        p:=Fscan-20;
      src:=copy(Fxml,p,50);
      result:= e.Message+' near '+src+cr+'Error position='+inttostr(Fscan)+cr+AsText;
    end;
  end;
  FIsParsing:=false;
end;


procedure TjanXMLParser2.parseAttributes(node: TjanXMLNode2; atts: string);
const BlankChars = [' ',#9,#10,#13,#0];
var
  s,attname,attvalue:string;
  p2,ps,pd:integer;
  IndexEqu, IndexAfterName, IndexValueDelimiter, AttrLen: integer;
  delim:char;
begin
  s:=trim(atts);
  while s<>'' do begin
    IndexEqu:=posstr('=',s);
    if IndexEqu=0 then
      raise exception.Create('missing = when parsing attributes');
    AttrLen := Length(s);
    IndexAfterName := IndexEqu;
    while (IndexAfterName>0) and (s[IndexAfterName] in BlankChars) do
      Dec(IndexAfterName);
    IndexValueDelimiter := IndexEqu+1;
    while (IndexValueDelimiter<AttrLen) and (s[IndexValueDelimiter] in BlankChars) do
      Inc(IndexValueDelimiter);
    delim:=s[IndexValueDelimiter];
    if not (delim in ['"','''']) then
      raise exception.Create('missing value delimiter when parsing attributes');
    p2:=posstr(delim,s,IndexValueDelimiter+1);
    if p2=0 then raise exception.Create('Expected closing '+delim+' when parsing attributes');
    attvalue:=copy(s,IndexValueDelimiter+1,p2-(IndexValueDelimiter+1));
    attname:=trim(copy(s,1,IndexAfterName-1));
    // unescape entities
    if posstr('&',attvalue)>0 then begin
      attvalue:=q_replacestr(attvalue,'&lt;','<');
      attvalue:=q_replacestr(attvalue,'&gt;','>');
      attvalue:=q_replacestr(attvalue,'&amp;','&');
      attvalue:=q_replacestr(attvalue,'&apos;','''');
      attvalue:=q_replacestr(attvalue,'&quot;','"');
    end;
    // JV-20040403
    //if FEscapeAttributes then
    //  attvalue:=janstrings.unescape(attvalue);

    //node.setAttribute(attname,attvalue);
    node.AddParsedAttribute(attname,attvalue); // JV-20040513
    delete(s,1,p2);
  end;
  if FAutoIndexByNameAndId then
    self.IndexNodeByNameAndId(node);
end;

{
procedure TjanXMLParser2.parseNode(rootNode,parentNode:TjanXMLNode2);
var
  p,p1,p2:integer;
  tag,tagname, strAttributes:string;
  bShortcut:boolean;
  newnode:TjanXMLNode2;
  upnode:TjanXMLNode2;
begin
//showmessage(copy(fxml,fscan,maxint));
  bShortcut:=false;
  fscan:=posstr('<',fxml,fscan);
  if fscan=0 then raise exception.Create('Missing <');
  // check for comment JV 20030724
  if copy(fxml,fscan,4)='<!--' then begin
    p:=posstr('-->',fxml,fscan);
    if p=0 then raise exception.Create('Missing comment terminator -->');
    fscan:=p+3;
    fscan:=posstr('<',fxml,fscan);
    if fscan=0 then raise exception.Create('Missing <');
  end;
  p:=posstr('>',fxml,fscan);
  if p=0 then raise exception.Create('Missing >');
  tag:=copy(fxml,fscan+1,p-fscan-1);
  if copy(tag,1,1)='/' then begin
    // closing tag
    FScan:=p+1;
    upnode:=parentNode.FParentNode;
    if (upNode=nil) then
      exit
    else if upNode=rootNode then begin
//      if posstr('<',fxml,fscan)>0 then
//      parseNode(rootNode,upnode);
      exit;
    end
    else begin
      parseNode(rootNode,upnode);
      exit;
    end;
  end;
  fscan:=p+1;
  // check for shortcut
  if copy(tag,length(tag),1)='/' then begin
    bShortCut:=true;
    tag:=trim(copy(tag,1,length(tag)-1));
  end
  else
   bShortCut:=false;
  // split tag and attributes
  p:=posstr(' ',tag);
  if p>0 then begin  // have attributes
    tagname:=copy(tag,1,p-1);
    strAttributes:=trim(copy(tag,p+1,maxint));
  end
  else begin
    tagname:=tag;
    strAttributes:='';
  end;
  if parentNode=nil then begin // xml root node
    newnode:=FparseRoot;
  end
  else if parentNode=rootNode then begin// fragment root node
    newnode:=FparseRoot;
  end
  else begin
    newnode:=TjanXMLNode2.create;
    newnode.FParser:=self;
    if parentnode<>nil then
      parentnode.FNodes.Add(newnode);
    newnode.FParentNode:=parentNode;
  end;
  newnode.name:=tagname;
  if strAttributes<>'' then
    parseAttributes(newnode,strAttributes);
  if bShortCut then begin
    upnode:=parentNode;
    if (upNode=nil) then
      exit
    else if (upNode=rootNode) then begin
//      if posstr('<',fxml,fscan)>0 then
//        parseNode(rootNode,upnode);
      exit
    end
    else begin
      parseNode(rootNode,upnode);
      exit;
    end;
  end;
  parseText(newnode);
  parseNode(rootNode,newnode);
end;}


function TjanXMLParser2.parseNode(rootNode,parentNode:TjanXMLNode2):boolean;
// modified JV-20040512
var
  p,p1,p2:integer;
  tag,tagname, strAttributes:string;
  bShortcut:boolean;
  newnode:TjanXMLNode2;
  upnode:TjanXMLNode2;
begin
  bShortcut:=false;
  fscan:=posstr('<',fxml,fscan);
  if fscan=0 then raise exception.Create('Missing <');

  // check for comment JV 20030724
  if copy(fxml,fscan,4)='<!--' then begin
    p:=posstr('-->',fxml,fscan);
    if p=0 then raise exception.Create('Missing comment terminator -->');
    fscan:=p+3;
    fscan:=posstr('<',fxml,fscan);
    if fscan=0 then raise exception.Create('Missing <');
  end;
  p:=posstr('>',fxml,fscan);
  if p=0 then raise exception.Create('Missing >');
  tag:=copy(fxml,fscan+1,p-fscan-1);
  result:= copy(tag,1,1)<>'/'; // JV-20040512
  if result then begin  //Assume trying to parse child node but have closing node
    fscan:=p+1; //accepted new node
    // check for shortcut
    if copy(tag,length(tag),1)='/' then begin
      bShortCut:=true;
      tag:=trim(copy(tag,1,length(tag)-1));
    end
    else
     bShortCut:=false;
    // split tag and attributes
    p:=posstr(' ',tag);
    if p>0 then begin  // have attributes
      tagname:=copy(tag,1,p-1);
      strAttributes:=trim(copy(tag,p+1,maxint));
    end
    else begin
      tagname:=tag;
      strAttributes:='';
    end;
    if parentNode=nil then begin // xml root node
      newnode:=FparseRoot;
    end
    else if parentNode=rootNode then begin// fragment root node
      newnode:=FparseRoot;
    end
    else begin
      newnode:=TjanXMLNode2.create;
      newnode.FParser:=self;
      if parentnode<>nil then
        parentnode.FNodes.Add(newnode);
      newnode.FParentNode:=parentNode;
    end;
    newnode.name:=tagname;
    if strAttributes<>'' then
      parseAttributes(newnode,strAttributes);
    if not bShortCut then begin
      parseText(newnode);
      repeat
      until not parseNode(rootNode, newnode); // all child tags, returns false without advancing parse position
      //Get end tag
      fscan:=PosStr('<',fxml,fscan);
      if fscan=0 then raise exception.Create('Missing <');
      p:=PosStr('>',fxml,fscan);
      if p=0 then raise exception.Create('Missing >');
      tag:=copy(fxml,fscan+1,p-fscan-1);
      if copy(tag,1,1)<>'/' then Raise Exception ('Not a matching end tag'+tag);
      tag:=copy(tag,2,length(tag)-1);
      if tag<>newnode.name then Raise Exception ('Opening and closing tags don''t match '+newnode.name+' '+tag);
      fscan:=p+1; //accepted new node
    end;
  end;
end;



procedure TjanXMLParser2.parseText(node: TjanXMLNode2);
var
  p1,p2:integer;
  s:string;
begin
  p1:=posstr('<',Fxml,fscan);
  if p1=0 then raise exception.Create('Expected < when parsing text');
  // check for <![CDATA[
  if copy(Fxml,p1,9)='<![CDATA[' then begin
    Fscan:=p1+9;
    p1:=posstr(']]>',Fxml,fscan);
    if p1=0 then raise exception.Create('Expected ]]> when parsing CDATA section');
    node.text:=trim(copy(Fxml,fscan,p1-fscan));
    Fscan:=p1+3;
  end
  else begin
    node.text:=trim(copy(Fxml,fscan,p1-Fscan));
    Fscan:=p1;
  end;
end;

procedure TjanXMLParser2.SaveXML(filename: string);
begin
  savestring(filename,self.AsText);
end;

procedure TjanXMLParser2.setXML(const Value: string);
begin
  Fxml:=value;
  FparseError:=parse;
end;



procedure TjanXMLParser2.XMLPut(value:string);
var
  L,oL:integer;
  tmp:string;
begin
  L:=length(value);
  if L=0 then exit;
  tmp:=value;
  if (FXMLPosition+L)>FXMLSize then begin
    if L>FpageSize then
      FXMLSize:=FXMLSize+L
    else
      FXMLSize:=FXMLSize+FpageSize;
    setlength(FOutPut,FXMLSize);
    FXMLP:=pointer(FOutPut);
    inc(FXMLP,FXMLPosition);
  end;
  System.Move(Pointer(tmp)^, FXMLP^, L);
  Inc(FXMLP, L);
  Inc(FXMLPosition,L);
end;


function TjanXMLParser2.transformNode_(node: TjanXMLNode2): string;
var
  templatelist,matchlist:TList;
  i,c:integer;
  nt,ncontext,templatenode:TjanXMLNode2;
  match:string;
  it,ct:integer;
  im,cm:integer;
begin
  FOutPut:='';
  FXMLSize:=FPageSize;
  FXMLPosition:=0;
  SetLength(FOutPut,FXMLSize);
  FXMLP:=pointer(FOutPut);
  FOutputdepth:=0;
  result:='';
  if self.name<>'xsl:stylesheet' then exit;
  try
    templatelist:=TList.Create;
    matchlist:=TList.Create;
    //JV 20031117
    //selectNodes(self,templatelist,'name=''xsl:template''');
    selectNodes(self,templatelist,'xsl:template');
    c:=templatelist.count;
    if c>0 then begin
      templatenode:=TjanXMLNode2(templatelist[0]);
      match:=templatenode.attribute['match'];
      ct:=templatenode.nodes.count;
      if match<>'' then begin
        if match='.' then
          matchlist.add(node)
        else
          node.selectNodes(self,matchlist,match);
        cm:=matchlist.count;
      end
      else
        cm:=0;
      if (cm<>0) and (ct<>0) then begin
        for im:=0 to cm-1 do begin
          ncontext:=TjanXMLNode2(matchlist[im]);
          for it:=0 to ct-1 do begin
            nt:=TjanXMLNode2(templatenode.nodes[it]);
            XSLTOutputNode(nt,ncontext,matchlist,templatelist);
          end;
        end;
      end;
    end;
  finally
    templatelist.free;
    matchlist.free;
  end;
  result:=copy(FOutput,1,FXMLPosition);
  FOutput:='';
end;

procedure TjanXMLParser2.XSLTOutputNode(node,context: TjanXMLNode2;matchlist,templatelist:TList);
var
  i,c:integer;
  att,attname,attvalue:string;
  ntext:string;
  v:variant;
  spc:string;
  atom:string;
  nodename:string;
  nchild:TjanXMLNode2;
  doatts:boolean;
  xpp:TjanXPathParser2;
begin
  nodename:=node.name;
  doatts:=true;
  if nodename='xsl:element' then begin
    nodename:=node.attribute['name'];
    if nodename='' then exit;
    doatts:=false;
    if nodename[1]='{' then begin
      if nodename[length(nodename)]<>'}' then exit;
      nodename:=copy(nodename,2,length(nodename)-2);
      xpp:=getXPathParser(nodename);
      xpp.XPath.CurrentNode:=context;
      v:= xpp.XPath.Evaluate;
      if v=null then exit;
      nodename:=v;
    end;
  end
//  else if copy(nodename,1,4)='xsl:' then begin
  else if node.namespace='xsl' then begin
    ExecXSLT(node,context,matchlist,templatelist);
    exit;
  end;
  if FoutputDepth=0 then
    spc:=''
  else
    spc:=stringofchar(' ',2*FoutputDepth);
  atom:=cr+spc+'<'+nodename;
  XMLPut(atom);
  c:=node.FAttributes.Count;
  if (c<>0) and doatts then begin
    att:='';
    for i:=0 to c-1 do begin
      attname:=TjanXMLAttribute2(node.FAttributes[i]).name;
      attvalue:=TjanXMLAttribute2(node.FAttributes[i]).value;
      if posstr('''',attvalue)>0 then
        att:=att+' '+attname+'="'+attvalue+'"'
      else
        att:=att+' '+attname+'='''+attvalue+'''';
    end;
    XMLPut(att);
  end;
  c:=node.FNodes.Count;
  ntext:=node.text;
  if (c=0) and (ntext='') then begin
    XMLPut('/>');
    exit;
  end;
//  atom:='>'+cdatacheck(ntext);
  if node.CDATA then // version 3.01
    atom:='><![CDATA['+ntext+']]>'
  else
    atom:='>'+ntext;
  if c=0 then begin
    //atom:=atom+cr+spc+'</'+nodename+'>';
    atom:=atom+'</'+nodename+'>';  // version 3.01
    XMLPut(atom);
    exit;
  end;
  inc(FOutputDepth);
  for i:=0 to c-1 do begin
    nchild:=TjanXMLNode2(node.Fnodes.items[i]);
    if nchild.name<>'xsl:attribute' then
     if atom<>'' then begin
       XMLPut(atom);
       atom:='';
     end;
     XSLTOutputNode(nchild,context,matchlist,templatelist);
  end;
  atom:=atom+cr+spc+'</'+nodename+'>';
  XMLPut(atom);
  dec(FOutputDepth);
end;

procedure TjanXMLParser2.xsl_call_template(node,context: TjanXMLNode2; matchlist,
  templatelist: TList);
var
  templatename:string;
  i,c:integer;
  ii,cc:integer;
  it,ct:integer;
  templateNode:TjanXMLNode2;
  match:string;
  matchlist_:TList;
  orderby:array of TjanXSLSort;

  function checksort:integer;
  var
    i,c:integer;
    nsort:TjanXMLNode2;
  begin
    result:=0;
    setlength(orderby,0);
    c:=node.nodes.count;
    if c=0 then exit;
    for i:=0 to c-1 do begin
      nsort:=TjanXMLNode2(node.nodes[i]);
      if nsort.name<>'xsl:sort' then continue;
      inc(result);
      setlength(orderby,result);
      orderby[result-1].pattern:=nsort.attribute['select'];
      orderby[result-1].SortAscending:=nsort.attribute['order']<>'descending';
      orderby[result-1].SortNumeric:=nsort.attribute['data-type']='number';
    end;
  end;
begin
  templatename:=node.attribute['name'];
  if templatename='' then exit;
  c:=templatelist.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    templateNode:=TjanXMLNode2(templatelist[i]);
    if templateNode.attribute['name']<>templatename then continue;
    match:=templateNode.attribute['match'];
    ct:=templatenode.nodes.count;
    if ct=0 then exit;
    matchlist_:=TList.create;
    try
      context.selectNodes(self,matchlist_,match);
      cc:=matchlist_.count;
      if cc>0 then begin
        if checksort<>0 then begin
          sort(matchlist_,0,cc,orderby);
        end;
        for ii:=0 to cc-1 do begin
          context:=TjanXMLNode2(matchlist_[ii]);
          for it:=0 to ct-1 do begin
            XSLTOutputNode(TjanXMLNode2(templatenode.nodes[it]),TjanXMLNode2(matchlist_[ii]),matchlist_,templatelist);
          end;
          //FXSLTOutput:=FXSLTOutput+cr;
          XMLPut(cr);
        end;
      end;
    finally
      matchlist_.free;
    end;
    exit;
  end;
end;

procedure TjanXMLParser2.xsl_if(node, context: TjanXMLNode2; matchlist,
  templatelist: TList);
var
  xpp:TjanXPathParser2;
  test:string;
  i,c:integer;
begin
  test:=node.attribute['test'];
  if test='' then exit;
  c:=node.nodes.count;
  if c=0 then exit;
  xpp:=getXPathParser(test);
  xpp.XPath.CurrentNode:=context;
  if xpp.XPath.Evaluate then
    for i:=0 to c-1 do begin
      XSLTOutputNode(TjanXMLNode2(node.Fnodes.items[i]),context,matchlist,templatelist);
    end;
end;

procedure TjanXMLParser2.xsl_value_of(node,context: TjanXMLNode2; matchlist,
  templatelist: TList);
var
  select:string;
  v:variant;
  n:TjanXMLNode2;
  tmp,axis,attri:string;
  p:integer;
  xpp:TjanXPathParser2;
begin
  n:=context;
  select:=node.attribute['select'];
  if select='' then exit;
  if select='.' then begin
    XMLPut(context.text);
    exit;
  end;
  xpp:=getXPathParser(select);
  xpp.XPath.CurrentNode:=context;
  v:= xpp.XPath.Evaluate;
  if v=null then exit;
  tmp:=v;
  XMLPut(tmp);
{  p:=pos('@',select);
  if p=0 then begin
    axis:=select;
  end
  else begin
    axis:=copy(select,1,p-1);
    attri:=copy(select,p+1,maxint);
  end;
  if (axis='.') or (axis='self::') then begin
    n:=context;
  end
  else if (axis='parent::') then begin
    n:=context.parentNode;
    if n=nil then exit;
  end
  else if axis='' then begin
    n:=context;
  end
  else if pos('::',axis)=0 then begin  // named child
    n:=context.getChildByName(axis);
    if n=nil then exit;
  end
  else if pos('child::',axis)=1 then begin
    delete(axis,1,7);
    n:=context.getChildByName(axis);
    if n=nil then exit;
  end;
  if attri<>'' then
    XMLPut(n.attribute[attri])
  else
    XMLPut(n.text);}
end;

procedure TjanXMLParser2.xsl_attribute(node, context: TjanXMLNode2;
  matchlist, templatelist: TList);
var
  tmp:string;
  c:integer;
  n:TjanXMLNode2;
begin
  tmp:=node.attribute['name'];
  c:=node.nodes.count;
  if c=0 then begin
    XMLPut(' '+tmp+'='''+node.text+'''');
  end
  else begin
    n:=TjanXMLNode2(node.nodes[0]);
    if n.name='xsl:value-of' then begin
      XMLPut(' '+tmp+'=''');
      xsl_value_of(n,context, matchlist, templatelist);
      XMLPut('''');
    end
    else
      XMLPut(' '+tmp+'=''''');
  end;
end;

procedure TjanXMLParser2.SetpageSize(const Value: integer);
begin
  if value>=$400 then
    FpageSize := Value;
end;

procedure TjanXMLParser2.xsl_comment(node, context: TjanXMLNode2;
  matchlist, templatelist: TList);
begin
  XMLPut('<!--'+node.text+'-->');
end;

procedure TjanXMLParser2.xsl_choose(node, context: TjanXMLNode2; matchlist,
  templatelist: TList);
var
  xpp:TjanXPathParser2;
  test:string;
  i,c, iw,cw:integer;
  n:TjanXMLNode2;
begin
  c:=node.nodes.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    n:=TjanXMLNode2(node.nodes[i]);
    if n.name='xsl:otherwise' then begin
      cw:=n.nodes.count;
      if cw=0 then exit;
      for iw:=0 to cw-1 do
        XSLTOutputNode(TjanXMLNode2(n.nodes.items[iw]),context,matchlist,templatelist);
      exit;
    end;
    if n.name<>'xsl:when' then continue;
    test:=n.attribute['test'];
    if test='' then continue;
    cw:=n.nodes.count;
    if cw=0 then continue;
    xpp:=getXPathParser(test);
    xpp.XPath.CurrentNode:=context;
    if xpp.XPath.Evaluate then begin
      for iw:=0 to cw-1 do
        XSLTOutputNode(TjanXMLNode2(n.nodes.items[iw]),context,matchlist,templatelist);
      exit;
    end;
  end
end;

procedure TjanXMLParser2.xsl_for_each(node, context: TjanXMLNode2;
  matchlist, templatelist: TList);
var
  templatename:string;
  i,c:integer;
  ii,cc:integer;
  it,ct:integer;
  templateNode:TjanXMLNode2;
  select:string;
  matchlist_:TList;
begin
  select:=node.attribute['select'];
  if select='' then exit;
  c:=node.nodes.count;
  if c=0 then exit;
  matchlist_:=TList.create;
  try
    context.selectNodes(self,matchlist_,select);
    cc:=matchlist_.count;
    if cc>0 then
      for ii:=0 to cc-1 do begin
        context:=TjanXMLNode2(matchlist_[ii]);
        for i:=0 to c-1 do begin
          XSLTOutputNode(TjanXMLNode2(node.nodes[i]),TjanXMLNode2(matchlist_[ii]),matchlist_,templatelist);
        end;
        XMLPut(cr);
      end;
  finally
    matchlist_.free;
  end;
end;

procedure TjanXMLParser2.Sort(matchlist: TList; From, Count: Integer;
  orderby: array of TjanXSLSort);
  procedure   Sort( iL, iR : Integer ) ;
  var
  	L, R, M : Integer ;
  begin
  	repeat
          	L := iL ;
              	R := iR ;
              	M := ( L + R ) div 2 ;

              	repeat
                  	while Compare(matchlist, From + L, From + M ,orderby) < 0 do Inc(L) ;
                  	while Compare(matchlist, From + M, From + R ,orderby) < 0 do Dec(R) ;
                  	if L <= R then begin
                      		Swap(matchlist, From + L, From + R ) ;
                      		if M = L then
                          		M := R
                      		else if M = R then
                          		M := L ;
                      		Inc(L) ;
                      		Dec(R) ;
                  	end ;
              	until L > R ;

              	if ( R - iL ) > ( iR - L ) then begin {Sort left here}
                  	if L < iR then
                      		Sort( L, iR ) ;
                  	iR := R ;
              	end else begin
                  	if iL < R then
                      		Sort( iL, R ) ;
                  	iL := L ;
              	end ;
          until iL >= iR ;
  end ;
begin
  if Count > 1 then
  	Sort( 0, Count - 1 ) ;
end;

function TjanXMLParser2.Compare(matchlist: TList; i, j: Integer;
  orderby: array of TjanXSLSort): Integer;
var
  v:variant;
  ni,nj:TjanXMLNode2;
  pattern,s1,s2:string;
  p:integer;
  tablename,fieldname:string;
  arecord:integer;
  obi,obc:integer;
  xpp:TjanXPathParser2;

  function safefloat(atext:string):double;
  begin
    try
      result:=strtofloat(atext);
    except
      result:=0;
    end;
  end;

  function comparefloats(afloat1,afloat2:double):integer;
  begin
    if afloat1=afloat2 then
      result:=0
    else if afloat1>afloat2 then
      result:=1
    else
      result:=-1;
  end;
begin
  result:=0;
  ni:=TjanXMLNode2(matchlist[i]);
  nj:=TjanXMLNode2(matchlist[j]);
  obc:=length(orderby);
  for obi:=0 to obc-1 do begin
    pattern:=orderby[obi].pattern;
    xpp:=getXPathParser(pattern);
    xpp.XPath.CurrentNode:=ni;
    v:=xpp.XPath.Evaluate;
    if v=null then
      s1:=ni.name
    else
      s1:=v;
    xpp.XPath.CurrentNode:=nj;
    v:=xpp.XPath.Evaluate;
    if v=null then
      s2:=nj.name
    else
      s2:=v;
    if orderby[obi].SortAscending then begin
      if orderby[obi].SortNumeric then
        result:=comparefloats(safefloat(s1),safefloat(s2))
      else
        result:=ansicomparestr(s1,s2);
      if result<>0 then break;
    end
    else begin
      if orderby[obi].SortNumeric then
        result:=comparefloats(safefloat(s2),safefloat(s1))
      else
        result:=ansicomparestr(s2,s1);
      if result<>0 then break;
    end
  end;
end;

procedure TjanXMLParser2.Swap(matchlist: TList; i, j: Integer);
begin
  matchlist.Exchange(i,j);
end;

procedure TjanXMLParser2.xsl_element(node, context: TjanXMLNode2;
  matchlist, templatelist: TList);
var
  tmp:string;
  v:variant;
  i,c:integer;
  n:TjanXMLNode2;
  xpp:TjanXPathParser2;
begin
  tmp:=node.attribute['name'];
  {$IFNDEF FPC}
showmessage(tmp);
  {$ENDIF}
  if tmp='' then exit;
  if tmp[1]='{' then begin
    if tmp[length(tmp)]<>'}' then exit;
    tmp:=copy(tmp,2,length(tmp)-2);
    xpp:=getXPathParser(tmp);
    xpp.XPath.CurrentNode:=context;
    v:= xpp.XPath.Evaluate;
    if v=null then exit;
    tmp:=v;
  end;
  c:=node.nodes.count;
  if c=0 then begin
    XMLPut('<'+tmp+' />');
    exit;
  end;
  XMLPut('<'+tmp);
  for i:=0 to c-1 do begin
    XSLTOutputNode(TjanXMLNode2(node.nodes[i]),context,matchlist,templatelist);
  end;
  XMLPut('</'+tmp+'>');
end;

procedure TjanXMLParser2.SetonXEL(const Value: TXELEvent);
begin
  FonXEL := Value;
end;

{function TjanXMLParser2.NodeXML(pNode: TjanXMLNode2): string;
var
  version,encoding:string;
begin
  result:='';
  if pNode=nil then exit;
  FOutPut:='';
  FXMLSize:=$10000;
  FXMLPosition:=0;
  SetLength(FOutPut,FXMLSize);
  FXMLP:=pointer(FOutPut);
  FOutputdepth:=0;
  version:=declaration.attribute['version'];
  if version='' then
    version:='1.0';
  encoding:=declaration.attribute['encoding'];
  if encoding='' then
    encoding:='ISO-8859-1';
  if pNode.parentNode=nil then
    XMLPut('<?xml version='''+version+''' encoding='''+encoding+''' ?>'+cr);
  OutputNode(pNode);
  result:=copy(FOutput,1,FXMLPosition);
  FOutput:='';
end;}

function TjanXMLParser2.getNodeXML(pNode: TjanXMLNode2): string;
var
  version,encoding:string;
begin
  result:='';
  if pNode=nil then exit;
  FOutPut:='';
  FXMLSize:=$10000;
  FXMLPosition:=0;
  SetLength(FOutPut,FXMLSize);
  FXMLP:=pointer(FOutPut);
  FOutputdepth:=0;
  version:=declaration.attribute['version'];
  if version='' then
    version:='1.0';
  encoding:=declaration.attribute['encoding'];
  if encoding='' then
    encoding:='ISO-8859-1';
  if pNode.parentNode=nil then
    XMLPut('<?xml version='''+version+''' encoding='''+encoding+''' ?>'+cr);
  OutputNode(pNode);
  result:=copy(FOutput,1,FXMLPosition);
  FOutput:='';
end;

procedure TjanXMLParser2.setNodeXML(pNode: TjanXMLNode2;
  const Value: string);
begin
  Fxml:=value;
  FparseError:=parseFragment(pNode);
end;


function TjanXMLParser2.parseFragment(pNode: TjanXMLNode2): string;
var
  src:string;
  p,p2:integer;
  atom:string;
begin
  Fscan:=1;
  FparseRoot:=pNode;
  if pNode=nil then begin
    result:='Invalid parameter pNode';
    exit;
  end;
  // clear any existing child nodes
  pNode.nodes.Clear;
  try
    parseNode(pNode.parentNode,pNode.parentNode);
    result:='';
  except
    on E: exception do begin
      if Fscan<21 then
        p:=1
      else
        p:=Fscan-20;
      src:=copy(Fxml,p,50);
      result:= e.Message+' near '+src+cr+'Error position='+inttostr(Fscan)+cr+AsText;
    end;
  end
end;

procedure TjanXMLParser2.xsl_apply_templates(node, context: TjanXMLNode2;
  matchlist, templatelist: TList);
var
  templatename:string;
  i,c:integer;
  ii,cc:integer;
  it,ct:integer;
  seli,selc:integer;
  templateNode, SelNode:TjanXMLNode2;
  select,match:string;
  selectlist_,matchlist_:TList;
  orderby:array of TjanXSLSort;

  function checksort:integer;
  var
    i,c:integer;
    nsort:TjanXMLNode2;
  begin
    result:=0;
    setlength(orderby,0);
    c:=node.nodes.count;
    if c=0 then exit;
    for i:=0 to c-1 do begin
      nsort:=TjanXMLNode2(node.nodes[i]);
      if nsort.name<>'xsl:sort' then continue;
      inc(result);
      setlength(orderby,result);
      orderby[result-1].pattern:=nsort.attribute['select'];
      orderby[result-1].SortAscending:=nsort.attribute['order']<>'descending';
      orderby[result-1].SortNumeric:=nsort.attribute['data-type']='number';
    end;
  end;
begin
  select:=node['select'];
  if select='' then exit;
  c:=templatelist.count;
  if c<2 then exit;
  selectlist_:=TList.create;
  matchlist_:=TList.create;
  context.selectNodes(self,selectlist_,select);
  selc:=selectlist_.Count;
  if selc=0 then begin  // no nodes selected
    selectlist_.free;
    matchlist_.free;
    exit;
  end;
  try
    // apply any order by
    if checksort<>0 then begin
      sort(selectlist_,0,selc,orderby);
    end;
    // now apply matching template to every node in selectlist_
    for seli:=0 to selc-1 do begin
      SelNode:=TjanXMLNode2(selectlist_[seli]);
      for i:=1 to c-1 do begin
        templateNode:=TjanXMLNode2(templatelist[i]);
        match:=templateNode.attribute['match'];
        ct:=templatenode.nodes.count;
        if ct=0 then continue;
        matchlist_.Clear;
        context.selectNodes(self,matchlist_,match);
        if matchlist_.IndexOf(SelNode)<>-1 then begin
          for it:=0 to ct-1 do begin
            XSLTOutputNode(TjanXMLNode2(templatenode.nodes[it]),SelNode,selectlist_,templatelist);
          end;
          XMLPut(cr);
          break;  // do not check following templates
        end;
      end;
    end;
  finally
    selectlist_.free;
    matchlist_.free;
  end;  
end;


function TjanXMLParser2.Validate(XsdSchema: TjanXMLParser2): boolean;
begin
  result:=false;
  // TODO

end;

procedure TjanXMLParser2.IndexNodes;
begin
  // clear the index
  FNodeIndex.Clear;
  IndexNode_(self);
end;

procedure TjanXMLParser2.IndexNode_(pNode: TjanXMLNode2);
var
  NodeId:string;
  i,c:integer;
begin
  NodeId:=pNode['id'];
  if NodeId<>'' then begin
    if FNodeIndex.Exists(NodeId) then
      raise exception.create('Found duplicate id '+NodeId);
    FNodeIndex[NodeId]:=pNode;
  end;
  c:=pNode.childCount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    IndexNode_(pNode.childNode[i]);
  end;
end;

function TjanXMLParser2.NodeFromId(pId: string): TjanXMLNode2;
begin
  if not FNodeIndex.Exists(pId) then
    result:=nil
  else
    result:=TjanXMLNode2(FNodeIndex[pId]);
end;

procedure TjanXMLParser2.NewDocumentFromSchema(Schema: TjanXMLParser2);
var
  xn:TjanXMLNode2;
begin
  xml:='<node/>';
  xn:=Schema.getChildByName('xs:element');
  if xn=nil then exit;
  NewNodeFromSchema(Schema,self,xn);
end;

procedure TjanXMLParser2.NewNodeFromSchema(Schema: TjanXMLParser2; docNode,
  schemaNode: TjanXMLNode2);
var
  mname,mtype:string;
  xtype:TjanXMLNode2;
begin
  mname:=schemaNode['name'];
  mtype:=schemaNode['type'];
  docNode.name:=mname;
  if pos('xs:',mtype)=0 then begin
    // find type
    xtype:=schema.getcomplexType(mtype);
    if xtype=nil then
      raise exception.create('Missing  schema type definition '+mtype);

  end;

end;

function TjanXMLParser2.getcomplexType(pType: string): TjanXMLNode2;
var
  i,c:integer;
  xn:TjanXMLNode2;
begin
  result:=nil;
  c:=childcount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xn:=childNode[i];
    if (xn.name='xs:complexType') and (xn['name']=pType) then begin
      result:=xn;
      break;
    end;
  end;
end;

function TjanXMLParser2.ValidateNode(XsdSchema: TjanXMLParser2;
  node: TjanXMLNode2): boolean;
var
  mname:string;
begin
  mname:=node.name;

end;

procedure TjanXMLParser2.IndexNodesByNameAndId;
begin
  // clear the index
  FIndexesByNameAndId.Clear;
  IndexNodeByNameAndId_(self);
end;

procedure TjanXMLParser2.IndexNodeByNameAndId_(pNode: TjanXMLNode2);
var
  NodeId:string;
  NodeName:string;
  i,c:integer;
  NameIndex:TobjectHash;
begin
  NodeId:=pNode['id'];
  NodeName:=pNode.name;
  pNode.FParser:=self;
  if NodeId<>'' then begin
    if FIndexesByNameAndId.Exists(NodeName) then
      NameIndex:=TObjectHash(FIndexesByNameAndId[NodeName])
    else begin
      NameIndex:=TObjectHash.Create;
      FIndexesByNameAndId[NodeName]:=NameIndex;
    end;
    if NameIndex.Exists(NodeId) then
      raise exception.create('Found duplicate id '+NodeId+' in name index '+NodeName);
    NameIndex[NodeId]:=pNode;
  end;
  c:=pNode.childCount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    IndexNodeByNameAndId_(pNode.childNode[i]);
  end;
end;

function TjanXMLParser2.GetNodeByNameAndId(pNodeName,
  pId: string): TjanXMLNode2;
var
  NameIndex:TobjectHash;
begin
  if not FIndexesByNameAndId.Exists(pNodeName) then
    result:=nil
  else begin
    NameIndex:=TobjectHash(FIndexesByNameAndId[pNodeName]);
    if not NameIndex.Exists(pId) then
      result:=nil
    else
      result:=TjanXMLNode2(NameIndex[pId]);
  end;
end;


procedure TjanXMLParser2.IndexNodeByNameAndId(pNode: TjanXMLNode2);
begin
  IndexNodeByNameAndId_(pNode);
end;

procedure TjanXMLParser2.SetAutoIndexByNameAndId(const Value: boolean);
begin
  FAutoIndexByNameAndId := Value;
end;

procedure TjanXMLParser2.RemoveNodeFromIndexByNameAndId(pNode: TjanXMLNode2);
var
  NodeId:string;
  NodeName:string;
  i,c:integer;
  NameIndex:TobjectHash;
begin
  NodeId:=pNode['id'];
  NodeName:=pNode.name;
  if NodeId<>'' then begin
    if FIndexesByNameAndId.Exists(NodeName) then begin
      NameIndex:=TObjectHash(FIndexesByNameAndId[NodeName]);
      if NameIndex.Exists(NodeId) then
        NameIndex.Delete(NodeId);
    end;
  end;  
end;

function TjanXMLParser2.DeleteNodeByNameAndId(pNodeName,
  pId: string): boolean;
var
  xn:TjanXMLNode2;
begin
  result:=false;
  xn:=GetNodeByNameAndId(pNodeName,pId);
  if xn=nil then exit;
  if xn.parentNode=nil then exit;
  xn.parentNode.deleteNode(xn);
  result:=true;
end;

function TjanXMLParser2.AddNodeByNameAndId(pNodeName, pId: string;
  pNode: TjanXMLNode2): boolean;
var
  xn:TjanXMLNode2;
begin
  result:=false;
  xn:=GetNodebyNameAndId(pNodeName,pId);
  if xn=nil then exit;
  xn.addNode(pNode);
  result:=true;
end;

{procedure TjanXMLParser2.SetEscapeAttributes(const Value: boolean);
begin
  FEscapeAttributes := Value;
end;}

{ TjanXPathParser2 }

constructor TjanXPathParser2.Create;
begin
  inherited;
  FXPath:=TjanXPathExpression2.create;
end;

destructor TjanXPathParser2.destroy;
begin
  FXPath.Free;
  inherited;
end;


procedure TjanXPathParser2.filterNodes(nodelist: TList);
var
  i,c:integer;
begin
  c:=nodelist.count;
  if c=0 then exit;
  XPath.CurrentList:=nodelist;
  for i:=c-1 downto 0 do begin
    XPath.CurrentNode:=TjanXMLNode2(nodelist[i]);
    if not XPath.Evaluate then begin
      nodelist.delete(i);
    end;
  end;
end;

procedure TjanXPathParser2.selectNodes(node:TjanXMLNode2;nodelist: TList;single:boolean=false);
var
  n:TjanXMLNode2;
  lis:TList;
  i,c:integer;
  b:boolean;
begin
  XPath.CurrentNode:=node;
  if XPath.Evaluate then begin
    nodelist.add(node);
    if single then exit;
  end;
  c:=node.nodes.count;
  if c=0 then exit;
  for i:=0 to c-1 do
    selectNodes(TjanXMLNode2(node.nodes[i]),nodelist,single);
end;



procedure TjanXPathParser2.SetCurrentNode(const Value: TjanXMLNode2);
begin
  FCurrentNode := Value;
end;


procedure TjanXPathParser2.Setpattern(const Value: string);
var
  tmp:string;
begin
  tmp:=value;
  // replace entities
  tmp:=janstrings.Q_ReplaceStr(tmp,'&lt;','<');
  tmp:=janstrings.Q_ReplaceStr(tmp,'&gt;','>');
  tmp:=janstrings.Q_ReplaceStr(tmp,'!=','<>');
  XPath.Expression:=tmp;
end;





function TjanXPathParser2.testNode(node: TjanXMLNode2): boolean;
begin
  XPath.CurrentNode:=node;
  result:=XPath.Evaluate;
end;

{ TjanXMLAttribute2 }

function TjanXMLAttribute2.cloneAttribute: TjanXMLAttribute2;
begin
  result:=TjanXMLAttribute2.Create;
  result.name:=name;
  result.value:=value;
end;


procedure TjanXMLAttribute2.Setname(const Value: string);
begin
  Fname := Value;
end;

procedure TjanXMLAttribute2.Setvalue(const Value: variant);
begin
  Fvalue := Value;
end;

{ TjanXMLFilter2 }





{ TjanXPathExpression2 }

{procedure TjanXPathExpression2.AddToken;
var
  tok:TToken;
begin
  tok:=TToken.Create;
  tok.name:=FToken;
  tok.tokenkind:=FTokenKind;
  tok.value:=FTokenValue;
  tok.operator:=FTokenOperator;
  tok.level:=FtokenLevel;
  tok.expression:=FTokenExpression;
  FInFix.Add(tok);
end;}

procedure TjanXPathExpression2.Clear;
begin
  ClearInfix;
  ClearPostFix;
  ClearStack;
end;

procedure TjanXPathExpression2.ClearInfix;
var
  i,c:integer;
begin
  c:=FInFix.Count;
  if c=0 then exit;
  for i:=c-1 downto 0 do
    TObject(FInFix.items[i]).free;
  FInFix.clear;
end;

procedure TjanXPathExpression2.ClearPostFix;
var
  i,c:integer;
begin
  c:=FPostFix.Count;
  if c=0 then exit;
  for i:=c-1 downto 0 do
    TObject(FPostFix.items[i]).free;
  FPostFix.clear;
end;

procedure TjanXPathExpression2.ClearStack;
var
  i,c:integer;
begin
  c:=FStack.Count;
  if c=0 then exit;
  for i:=c-1 downto 0 do
    TObject(FStack.items[i]).free;
  FStack.clear;
end;
{
For each token in INPUT do the following:

If the token is an operand, enqueue it in OUTPUT.

If the token is an open bracket - push it on STACK.

If the token is a closing bracket:
  - pop operators off STACK and enqueue them in OUTPUT,
  until you encounter an open bracket.
  Discard the opening bracket. If you reach the bottom of STACK without seeing an open bracket this indicates that the parentheses in the infix expression do not match, and so you should indicate an error.

If the token is an operator - pop operators off STACK and enqueue them in OUTPUT, until one of the following occurs:
- STACK is empty
- the operator at the top of STACK has lower precedence than the token
- the operator at the top of the stack has the same precedence as the token and the token is right associative.
Once you have done that push the token on STACK.

When INPUT becomes empty pop any remaining operators from STACK and enqueue them in OUTPUT. If one of the operators on STACK happened to be an open bracket, that means that its closing bracket never came, so an an error should be indicated.
}
function TjanXPathExpression2.ConvertInFixToPostFix: boolean;
var
  i,c,level:integer;
  tok:TToken;
begin
  result:=false;
  c:=FInfix.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    tok:=TToken(FInfix[i]);
    case tok.tokenkind of
    tkOperand: if not InFixToPostFix(i) then exit;
    tkOpen: if not InFixToStack(i) then exit;
    tkClose: if not CloseStackToPostFix then exit;
    tkOperator:
      begin
        if not OperatorsToPostFix(tok.level) then exit;
        InFixToStack(i);
      end;
    end;
  end;
  result:=FlushStackToPostFix;
end;

{
If the token is a closing bracket:
  - pop operators off STACK and enqueue them in OUTPUT,
  until you encounter an open bracket.
  Discard the opening bracket. If you reach the bottom of STACK without seeing an open bracket this indicates that the parentheses in the infix expression do not match, and so you should indicate an error.

}
function TjanXPathExpression2.CloseStackToPostFix: boolean;
begin
  result:=false;
  while (FStack.count<>0) and (TToken(Fstack[FStack.count-1]).tokenkind<>tkOpen) do
    StackToPostFix;
  if FStack.count<>0 then begin
    TToken(FStack[FStack.count-1]).free;
    Fstack.Delete(FStack.count-1);
    result:=true;
  end;
end;

{
If the token is an operator - pop operators off STACK and enqueue them in OUTPUT, until one of the following occurs:
- STACK is empty
- the operator at the top of STACK has lower precedence than the token
- the operator at the top of the stack has the same precedence as the token and the token is right associative.
Once you have done that push the token on STACK.
}
function TjanXPathExpression2.OperatorsToPostFix(Level:integer): boolean;
begin
  while (FStack.count<>0) and (TToken(Fstack[FStack.count-1]).level>=level) do
    StackToPostFix;
  result:=true;
end;

{
When INPUT becomes empty pop any remaining operators from STACK and enqueue them in OUTPUT. If one of the operators on STACK happened to be an open bracket, that means that its closing bracket never came, so an an error should be indicated.
}
function TjanXPathExpression2.FlushStackToPostFix: boolean;
begin
  result:=false;
  while (FStack.count<>0) and (TToken(Fstack[FStack.count-1]).tokenkind<>tkOpen) do
    StackToPostFix;
  result:=FStack.count=0;
end;

constructor TjanXPathExpression2.Create;
begin
  FInFix:=TList.create;
  FPostFix:=TList.create;
  FStack:=TList.create;
  CurrentNode:=nil;
  onGetAttribute:=GetAttribute;
end;

destructor TjanXPathExpression2.Destroy;
begin
  Clear;
  FInFix.free;
  FPostFix.free;
  Fstack.free;
  inherited;
end;

procedure TjanXPathExpression2.getInFix(list: TStrings);
var
  i,c:integer;
begin
  list.Clear;
  c:=FInFix.Count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    list.append(TToken(FInFix[i]).name);
  end;
end;

procedure TjanXPathExpression2.getPostFix(list:Tstrings);
var
  i,c:integer;
begin
  list.Clear;
  c:=FPostFix.Count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    list.append(TToken(FPostFix[i]).name);
  end;
end;



function TjanXPathExpression2.InfixToPostFix(index: integer): boolean;
begin
  result:=false;
  if (index<0) or (index>=FInFix.count) then exit;
  FPostFix.add(TToken(FInfix[index]).copy);
  result:=true;
end;


function TjanXPathExpression2.InFixToStack(index: integer): boolean;
begin
  result:=false;
  if (index<0) or (index>=FInFix.count) then exit;
  FStack.add(TToken(FInfix[index]).copy);
  result:=true;
end;

function TjanXPathExpression2.Parse;
var
  tokenizer:TjanXPathTokenizer;
begin
  clear;
  try
    tokenizer:=TjanXPathTokenizer.create;
    result:=Tokenizer.Tokenize(FSource,FInfix);
  finally
    tokenizer.free;
  end;
end;

procedure TjanXPathExpression2.procAdd;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2 + v1);
end;

procedure TjanXPathExpression2.procAnd;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2 and v1);
end;

procedure TjanXPathExpression2.procDivide;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2/v1);
end;

procedure TjanXPathExpression2.procEq;
var
  v1,v2:variant;
  b:boolean;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2=v1);
end;

procedure TjanXPathExpression2.procGe;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2>=v1);
end;

procedure TjanXPathExpression2.procGt;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2>v1);
end;

procedure TjanXPathExpression2.procLe;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2<=v1);
end;

procedure TjanXPathExpression2.procLt;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2<v1);
end;

procedure TjanXPathExpression2.procMultiply;
begin
  runpush(runpop* runpop);
end;

procedure TjanXPathExpression2.procNe;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2<>v1);
end;

procedure TjanXPathExpression2.procNumber;
begin
  runpush(TToken(FPostFix[FPC]).value);
end;

procedure TjanXPathExpression2.procOr;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2 or v1);
end;

procedure TjanXPathExpression2.procString;
begin
  runpush(TToken(FPostFix[FPC]).value);

end;

procedure TjanXPathExpression2.procSubtract;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(v2-v1);
end;

procedure TjanXPathExpression2.Setsource(const Value: string);
begin
  Fsource := Value;
  SL:=length(FSource);
  parse;
  ConvertInFixToPostFix;
end;

function TjanXPathExpression2.StackToPostFix: boolean;
var
  tok:TToken;
begin
  result:=false;
  if FStack.count=0 then exit;
  tok:=TToken(FStack[FStack.count-1]);
  FPostFix.Add(tok);
  FStack.Delete(FStack.count-1);
  result:=true;
end;

procedure TjanXPathExpression2.runOperator(op:TTokenOperator);
begin
  case op of
    toAssign: procAssign;
    toGet: procGet;
    toString: procString;
    toNumber: procNumber;
    toAttribute:procAttribute;
    toName: procName;
    toPosition: procPosition;
    toFirst:procFirst;
    toLast:procLast;
    toPath: procPath;
    toLocation: procLocation;
    toBaseName: procBaseName;
    toParentName: procParentName;
    toNameSpace: procNameSpace;
    toNameSpaceURI: procNameSpaceURI;
    toValue: procValue;
    toChildCount: procChildCount;
    toHasAttribute:procHasAttribute;
    toHasChild:procHasChild;
    toEq: procEq;
    toNe: procNe;
    toGt: procGt;
    toGe: procGe;
    toLt: procLt;
    toLe: procLe;
    toAdd: procAdd;
    toSubtract: procSubtract;
    toMultiply: procMultiply;
    toDivide: procDivide;
    toAnd: procAnd;
    toOr: procOr;
    toNot: procNot;
    toLike: procLike;
    toIn:procIn;
    toSin: procSin;
    toCos: procCos;
    toSqr: procSqr;
    toSqrt: procSqrt;
    toUPPER: procUPPER;
    toLOWER: procLOWER;
    toTRIM: procTRIM;
    toSoundex: procSoundex;
    toLeft:procLeft;
    toRight:procRight;
    toMid:procMid;
    toLen:procLen;
    toFix:procFix;
    toCeil:procCeil;
    toFloor:procFloor;
    toAsNumber: procAsNumber;
    toParseFloat: procParseFloat;
    toAsDate: procAsDate;
    toFormat: procFormat;
    toYear: procYear;
    toMonth: procMonth;
    toDay: procDay;
    toDateAdd: procDateAdd;
    toEaster: procEaster;
    toWeekNumber:procWeekNumber;
    toIsNumeric: procIsNumeric;
    toIsDate: procIsDate;
    toReplace:procReplace;
    toSubstr_After:procSubstr_After;
    toSubstr_Before:procSubstr_Before;
  end;
end;

function TjanXPathExpression2.Evaluate: variant;
var
  i,c:integer;
  op:TTokenOperator;
begin
  result:=null;
  c:=FPostFix.Count;
  if c=0 then exit;
  SP:=0;
  for i:=0 to c-1 do begin
    FPC:=i;
    op:=TToken(FPostFix[i]).operator;
    try
      runoperator(op);
    except
      exit;
    end;
  end;
  result:=runpop;
end;



function TjanXPathExpression2.runpop: variant;
begin
  if SP=0 then
    result:=null
  else begin
    dec(SP);
    result:=Vstack[sp];
  end;

end;

procedure TjanXPathExpression2.runpush(value: variant);
begin
  VStack[SP]:=value;
  inc(SP);
end;


procedure TjanXPathExpression2.SetonGetVariable(const Value: TVariableEvent);
begin
  FonGetVariable := Value;
end;


procedure TjanXPathExpression2.procSin;
var
  v1:variant;
begin
  v1:=runpop;
  runpush(sin(v1));
end;

procedure TjanXPathExpression2.procNot;
var
  v1:variant;
begin
  v1:=runpop;
  runpush(not(v1));
end;

procedure TjanXPathExpression2.procLike;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(IsLike(v1,v2));
end;

function TjanXPathExpression2.IsLike(v1, v2: variant): boolean;
var
  p1,p2:integer;
  s1,s2:string;
begin
  result:=false;
  s1:=v1;
  s2:=v2;
  if posstr('%',s1)=0 then begin
    result:=ansisametext(s1,s2)
  end
  else if (copy(s1,1,1)='%') and (copy(s1,length(s1),1)='%') then begin
    s1:=copy(s1,2,length(s1)-2);
    result:=postext(s1,s2)>0;
  end
  else if (copy(s1,1,1)='%') then begin
    s1:=copy(s1,2,maxint);
    p1:=postext(s1,s2);
    result:=p1=length(s2)-length(s1)+1;
  end
  else if (copy(s1,length(s1),1)='%') then begin
    s1:=copy(s1,1,length(s1)-1);
    result:=postext(s1,s2)=1;
  end;
end;






procedure TjanXPathExpression2.GetTokenList(list: TList; from,
  till: integer);
var
  tok:TToken;
  i:integer;

begin
  Clear;
  for i:=from to till do
    FInFix.Add(TToken(list[i]).copy);
  ConvertInFixToPostFix;
end;

procedure TjanXPathExpression2.procLOWER;
var
  v1:variant;
  s1:string;
begin
  v1:=runpop;
  s1:=v1;
  runpush(lowercase(s1));
end;

procedure TjanXPathExpression2.procTRIM;
var
  v1:variant;
  s1:string;
begin
  v1:=runpop;
  s1:=v1;
  runpush(trim(s1));
end;

procedure TjanXPathExpression2.procUPPER;
var
  v1:variant;
  s1:string;
begin
  v1:=runpop;
  s1:=v1;
  runpush(uppercase(s1));
end;

procedure TjanXPathExpression2.procSoundex;
var
  v1:variant;
  s1:string;
begin
  v1:=runpop;
  s1:=v1;
  runpush(soundex(s1));
end;

procedure TjanXPathExpression2.procAsNumber;
var
  v1:variant;
  s1:string;
  d1:double;
begin
  v1:=runpop;
  try
    s1:=v1;
    v1:=strtofloat(s1);
  except
    v1:=0;
  end;
  s1:=v1;
  runpush(v1);
end;

procedure TjanXPathExpression2.procLeft;
var
  asize,atext:variant;
  s1:string;
  p:integer;
begin
  asize:=runpop;
  atext:=runpop;
  s1:=atext;
  p:=asize;
  s1:=copy(s1,1,p);
  runpush(s1);
end;

procedure TjanXPathExpression2.procRight;
var
  asize,atext:variant;
  s1:string;
  p:integer;
begin
  asize:=runpop;
  atext:=runpop;
  s1:=atext;
  p:=asize;
  s1:=copy(s1,length(s1)-p+1,p);
  runpush(s1);
end;

procedure TjanXPathExpression2.procMid;
var
  vcount,vfrom,vtext:variant;
  s1:string;
  p,c:integer;
begin
  vcount:=runpop;
  vfrom:=runpop;
  vtext:=runpop;
  s1:=vtext;
  p:=vfrom;
  c:=vcount;
  s1:=copy(s1,p,c);
  runpush(s1);
end;

procedure TjanXPathExpression2.procCos;
var
  v1:variant;
begin
  v1:=runpop;
  runpush(cos(v1));
end;

procedure TjanXPathExpression2.procSqr;
var
  v1:variant;
begin
  v1:=runpop;
  runpush(sqr(v1));
end;



procedure TjanXPathExpression2.procSqrt;
var
  v1:variant;
begin
  v1:=runpop;
  runpush(sqrt(v1));
end;

procedure TjanXPathExpression2.procLen;
var
  v1:variant;
  s1:string;
begin
  v1:=runpop;
  s1:=v1;
  runpush(length(s1));
end;

procedure TjanXPathExpression2.procFix;
var
  vfloat,vdecimals:variant;
  s1,s2:string;
  d1:double;
begin
  vdecimals:=runpop;
  vfloat:=runpop;
  s1:=vfloat;
  s2:=vdecimals;
  try
    d1:=strtofloat(s1);
    s1:=format('%.'+s2+'f',[d1]);
  except
  end;
  runpush(s1);
end;

procedure TjanXPathExpression2.procCeil;
var
  v1:variant;
begin
  v1:=runpop;
  runpush(ceil(v1));
end;

procedure TjanXPathExpression2.procFloor;
var
  v1:variant;
begin
  v1:=runpop;
  runpush(floor(v1));
end;

procedure TjanXPathExpression2.procFormat;
var
  vfloat,vformat:variant;
  s1,s2:string;
  d1:double;
  i1:integer;
begin
  vformat:=runpop;
  vfloat:=runpop;
  s1:=vfloat;
  s2:=vformat;
  if s2='' then begin
    runpush(s1);
    exit;
  end;
  if s2[length(s2)] in ['d','x'] then
  try
    i1:=strtoint(s1);
    s1:=format(s2,[i1]);
  except
  end
  else if s2[length(s2)] in ['s'] then
  try
    s1:=format(s2,[s1]);
  except
  end
  else
  try
    d1:=strtofloat(s1);
    s1:=format(s2,[d1]);
  except
  end;
  runpush(s1);
end;


procedure TjanXPathExpression2.procDay;
{return the day part as integer from a 'yyyy-mm-dd' string}
var
  v1:variant;
  s1:string;
  i1:integer;
  adate:TDate;
begin
  v1:=runpop;
  s1:=v1;
  try
    adate:=strtodate(s1);
    i1:=Date2Day(aDate);
  except
    i1:=0;
  end;
  runpush(i1);
end;

procedure TjanXPathExpression2.procMonth;
{return the month part as integer from a 'yyyy-mm-dd' string}
var
  v1:variant;
  s1:string;
  i1:integer;
  adate:TDate;
begin
  v1:=runpop;
  s1:=v1;
  try
    adate:=strtodate(s1);
    i1:=Date2Month(aDate);
  except
    i1:=0;
  end;
  runpush(i1);
end;

procedure TjanXPathExpression2.procYear;
{return the year part as integer from a 'yyyy-mm-dd' string}
var
  v1:variant;
  s1:string;
  i1:integer;
  aDate:Tdate;
begin
  v1:=runpop;
  s1:=v1;
  try
    aDate:=strtodate(s1);
    i1:=Date2Year(aDate);
  except
    i1:=0;
  end;
  runpush(i1);
end;

procedure TjanXPathExpression2.procDateAdd;
{add number of intervals to date}
var
  vinterval,vnumber,vdate:variant;
  ayear,amonth,aday:word;
  adate:TDateTime;
  sinterval,sdate:string;
  inumber:integer;
begin
  vdate:=runpop;
  vnumber:=runpop;
  vinterval:=runpop;
  sinterval:=lowercase(vinterval);
  inumber:=vnumber;
  sdate:=vdate;
  try
    adate:=strtodate(sdate);
    decodedate(adate,ayear,amonth,aday);
    adate:=encodedate(ayear,amonth,aday);
    if sinterval='d' then
      adate:=adate+1
    else if sinterval='m' then
      adate:=incmonth(adate,inumber)
    else if sinterval='y' then
      adate:=encodedate(ayear+inumber,amonth,aday)
    else if sinterval='w' then
      adate:=adate+7*inumber
    else if sinterval='q' then
      adate:=incmonth(adate,inumber*3);
    sdate:=datetostr(adate);
  except
  end;
  runpush(sdate);
end;


procedure TjanXPathExpression2.procEaster;
// returns the easter date of a given year
var
  vyear:variant;
  ayear:integer;
  s1:string;
  adate:TDateTime;
begin
  vyear:=runpop;
  s1:='';
  try
    ayear:=vyear;
    s1:=datetostr(easter(ayear));
  except
  end;
  runpush(s1);
end;

procedure TjanXPathExpression2.procWeekNumber;
var
  v1:variant;
  s1:string;
  i1:integer;
  d1:TDateTime;
begin
  v1:=runpop;
  try
    s1:=v1;
    d1:=strtodate(s1);
    i1:=Date2WeekNo(d1);
  except
    i1:=0;
  end;
  runpush(i1);
end;

procedure TjanXPathExpression2.procIsNumeric;
var
  v1:variant;
  s1:string;
  d1:extended;
begin
  v1:=runpop;
  s1:=v1;
  try
    d1:=strtofloat(s1);
    runpush(true)
  except
    runpush(false)
  end;
end;

procedure TjanXPathExpression2.procIsDate;
var
  v1:variant;
  s1:string;
  d1:extended;
begin
  v1:=runpop;
  s1:=v1;
  runpush(SQLStringToDate(s1)<>0);
end;

procedure TjanXPathExpression2.procReplace;
// replace(source, oldpattern, newpattern)
var
  vsource, vold, vnew:variant;
  ssource, sold, snew:string;
begin
  vnew:=runpop;
  vold:=runpop;
  vsource:=runpop;
  ssource:=vsource;
  sold:=vold;
  snew:=vnew;
  ssource:=stringreplace(ssource,sold,snew,[rfreplaceall,rfignorecase]);
  runpush(ssource);
end;

procedure TjanXPathExpression2.procsubstr_after;
var
  vsource,vsubstr:variant;
  ssubstr,ssource,s1:string;
  p:integer;
begin
  vsubstr:=runpop;
  vsource:=runpop;
  ssubstr:=vsubstr;
  ssource:=vsource;
  p:=postext(ssubstr,ssource);
  if p>0 then
    s1:=copy(ssource,p+length(ssubstr),maxint)
  else
    s1:='';
  runpush(s1);
end;

procedure TjanXPathExpression2.procsubstr_before;
var
  vsource,vsubstr:variant;
  ssubstr,ssource,s1:string;
  p:integer;
begin
  vsubstr:=runpop;
  vsource:=runpop;
  ssubstr:=vsubstr;
  ssource:=vsource;
  p:=postext(ssubstr,ssource);
  if p>0 then
    s1:=copy(ssource,1,p-1)
  else
    s1:='';
  runpush(s1);
end;


procedure TjanXPathExpression2.procAttribute;
var
  AttributeName:string;
  AttributeValue:Variant;
  handled:boolean;
begin
  AttributeName:=TToken(FPostFix[FPC]).name;
  if assigned(onGetAttribute) then begin
    handled:=false;
    onGetAttribute(self,AttributeName,AttributeValue,handled);
    if not handled then
     AttributeValue:=AttributeName;
  end
  else
    AttributeValue:=AttributeName;
  runpush(AttributeValue);
end;


procedure TjanXPathExpression2.SetCurrentNode(const Value: TjanXMLNode2);
begin
  FCurrentNode := Value;
end;

procedure TjanXPathExpression2.GetAttribute(sender: Tobject;
  const VariableName: string; var VariableValue: variant;
  var handled: boolean);
begin
  if CurrentNode=nil then begin
    VariableValue:='';
    handled:=true;
  end;
  if CurrentNode.hasAttribute(variablename) then begin
    variableValue:=CurrentNode.attribute[variablename];
  end
  else
    variableValue:='';
  handled:=true;
end;

{procedure TjanXPathExpression2.GetElement(sender: Tobject;
  const VariableName: string; var VariableValue: variant;
  var handled: boolean);
begin
//
end;}

procedure TjanXPathExpression2.procName;
begin
  if CurrentNode=nil then begin
    runpush('');
  end
  else begin
    runpush(CurrentNode.name);
  end;
end;

procedure TjanXPathExpression2.procValue;
begin
  if CurrentNode=nil then
    runpush('')
  else
    runpush(CurrentNode.Text);
end;

procedure TjanXPathExpression2.procParentName;
begin
  if CurrentNode=nil then begin
    runpush('');
  end
  else begin
    if CurrentNode.parentNode<>nil then
      runpush(CurrentNode.ParentNode.name)
    else
      runpush('');
  end;
end;

procedure TjanXPathExpression2.procChildCount;
begin
  if CurrentNode=nil then begin
    runpush(0);
  end
  else begin
    runpush(CurrentNode.nodes.Count);
  end;
end;

procedure TjanXPathExpression2.procHasAttribute;
var
  s:string;
begin
  s:=runpop;
  if CurrentNode=nil then
    runpush(false)
  else
    runpush(CurrentNode.hasAttribute(s));
end;

procedure TjanXPathExpression2.procHasChild;
var
  s:string;
  i,c:integer;
begin
  s:=runpop;
  if CurrentNode=nil then
    runpush(false)
  else if CurrentNode.nodes.count=0 then
    runpush(false)
  else begin
    c:=CurrentNode.nodes.count;
    for i:=0 to c-1 do
      if TjanXMLNode2(CurrentNode.nodes[i]).name=s then begin
        runpush(true);
        exit;
      end;
    runpush(false);
  end;
end;

procedure TjanXPathExpression2.procAsDate;
var
  v1:variant;
  s1:string;
  d1:double;
begin
  v1:=runpop;
  try
    s1:=v1;
    v1:=strtodate(s1);
  except
    v1:=0;
  end;
  s1:=v1;
  runpush(v1);
end;

procedure TjanXPathExpression2.procIn;
var
  v1,v2:variant;
begin
  v1:=runpop;
  v2:=runpop;
  runpush(IsIn(v2,v1));
end;

function TjanXPathExpression2.IsIn(v1, v2: variant): boolean;
var
  p1,p2:integer;
  s1,s2:string;
begin
  s1:=v1;
  s2:=v2;
  s2:=','+s2+',';

  result:=pos(','+s1+',',s2)>0;
end;



{ TjanXPathParserList2 }

procedure TjanXPathParserList2.Clear;
var
  i,c:integer;
begin
  c:=self.Count;
  if c>0 then
    for i:=0 to c-1 do
      TjanXPathParser2(self.objects[i]).free;
  inherited;
end;

destructor TjanXPathParserList2.destroy;
begin
  clear;
  inherited;
end;

procedure TjanXPathExpression2.procParseFloat;
var
  v1:variant;
  s1:string;
  d1:double;

  function parseFloat(s:string):string;
  var
    i:integer;
  begin
    result:='';
    if s='' then exit;
    for i:=1 to length(s) do
      if s[i] in ['0'..'9','-','+','.'] then
        result:=result+s[i];
  end;
begin
  v1:=runpop;
  try
    s1:=v1;
    v1:=strtofloat(parsefloat(s1));
  except
    v1:=0;
  end;
  runpush(v1);
end;

procedure TjanXPathExpression2.procNameSpace;
begin
  if CurrentNode=nil then
    runpush('')
  else
    runpush(CurrentNode.namespace);
end;

procedure TjanXPathExpression2.procBaseName;
var
  nodename:string;
  p:integer;
begin
  if CurrentNode=nil then
    runpush('')
  else begin
    nodename:=CurrentNode.name;
    p:=pos(':',nodename);
    if p=0 then
      runpush(nodename)
    else
      runpush(copy(nodename,p+1,maxint));
  end;
end;

procedure TjanXPathExpression2.procNameSpaceURI;
var
  ns:string;
begin
  if CurrentNode=nil then
    runpush('')
  else
    runpush(CurrentNode.namespaceURI);
end;

procedure TjanXPathExpression2.procAssign;
var
  vvariable,vvalue:variant;
  n:TjanXMLNode2;
  procresult, stopsearch:boolean;
begin
// lookup first matching variable
  vvalue:=runpop;
  vvariable:=runpop;
  procresult:=false;
  if CurrentNode<>nil then begin
    n:=CurrentNode.GetXELVariable(vvariable);
    if n<>nil then begin
      n.text:=vvalue;
      procresult:=true;
    end;
  end;
  runpush(procresult);
end;

procedure TjanXPathExpression2.procGet;
var
  vvariable:variant;
  n:TjanXMLNode2;
  procresult:string;
begin
  procresult:='';
  vvariable:=runpop;
  if CurrentNode<>nil then begin
    n:=CurrentNode.getXELVariable(vvariable);
    if n<>nil then
      procresult:=n.text;
  end;
  runpush(procresult);
end;

{ TjanXMLMenuItem }
{$IFNDEF FPC}
procedure TjanXMLMenuItem2.SetNode(const Value: TJanXMLNode2);
begin
  FNode := Value;
end;
{$ENDIF}

procedure TjanXPathExpression2.procPath;
var
  xn:TjanXMLNode2;
  temp:string;
begin
  if CurrentNode=nil then begin
    runpush('');
  end
  else begin
    xn:=CurrentNode;
    temp:=CurrentNode.name;
    while xn.parentNode<>nil do begin
      xn:=xn.parentNode;
      temp:=xn.name+'\'+temp;
    end;
    runpush(temp);
  end;
end;

procedure TjanXPathExpression2.procLocation;
var
  xn, npath:TjanXMLNode2;
  XPath,temp,attri, axis, nodeName:string;
  locationStep, XPathValue:string;
  p:integer;
  descend:boolean;
begin
  if CurrentNode=nil then begin
    runpush('');
  end
  else begin
    xn:=CurrentNode;
    XPath:=TToken(FPostFix[FPC]).name;
    temp:=XPath;
    if temp[1]='/' then begin
      xn:=xn.rootNode;
      delete(temp,1,1);
    end;
    p:=pos('@',temp);
    if p=0 then begin
      attri:='';
    end
    else begin
      attri:=copy(temp,p+1,maxint);
      temp:=copy(temp,1,p-1);
    end;
    while temp<>'' do begin
      if temp[1]='/' then begin
        descend:=true;
        delete(temp,1,1);
        if temp='' then
          raise exception.create('Invalid // at the end of XPath '+XPath);
      end
      else
        descend:=false;
      p:=pos('/',temp);
      if p>0 then begin
        LocationStep:=copy(temp,1,p-1);
        temp:=copy(temp,p+1,maxint);
      end
      else begin
        LocationStep:=temp;
        temp:='';
      end;
      p:=pos('::',LocationStep);
      if p>0 then begin
        axis:=copy(LocationStep,1,p-1);
        NodeName:=copy(LocationStep,p+2,maxint);
      end
      else begin
        axis:='child';
        NodeName:=LocationStep;
      end;
      if (nodeName='..') or (axis='parent') then
        xn:=xn.parentNode;
      if xn=nil then
        raise exception.create('Can not locate parentnode in location '+XPath);
      if (nodeName<>'') and (nodeName<>'..') then
        xn:=xn.getChildByName(nodeName,descend);
      if xn=nil then
        raise exception.create('Can not locate '+nodeName+' in location '+XPath);
    end;
    if attri='' then
      runpush(xn.Text)
    else
      runpush(xn.attribute[attri]);
  end;
end;

procedure TjanXPathExpression2.procPosition;
begin
  if CurrentNode=nil then begin
    runpush(-1);
  end
  else begin
    if FCurrentList<>nil then
      runpush(FCurrentList.IndexOf(FCurrentNode))
    else
      runpush(-1);
  end;
end;


procedure TjanXPathExpression2.SetCurrentList(const Value: TList);
begin
  FCurrentList := Value;
end;

procedure TjanXPathExpression2.procLast;
begin
  if CurrentNode=nil then begin
    runpush(false);
  end
  else begin
    if FCurrentList<>nil then
      runpush(FCurrentList.IndexOf(FCurrentNode)=(FCurrentList.count-1))
    else
      runpush(false);
  end;
end;

procedure TjanXPathExpression2.procFirst;
begin
  if CurrentNode=nil then begin
    runpush(false);
  end
  else begin
    if FCurrentList<>nil then
      runpush(FCurrentList.IndexOf(FCurrentNode)=0)
    else
      runpush(false);
  end;
end;

{ TjanXSDParser2 }

procedure TjanXSDParser2.AddAttributes(ptypenode: TjanXMLNode2;
  var pelementnode: TjanXMLNode2);
var
  i,c:integer;
  xnc,xt,xfirst,xenum:TjanXMLNode2;
  mtype,aname:string;
begin
  c:=ptypenode.childCount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xnc:=ptypenode.childNode[i];
    if xnc.name<>'xs:attribute' then continue;
    aname:=xnc['name'];
    mtype:=xnc['type'];
    if posstr('xs:',mtype)>0 then begin
      if mtype='xs:string' then
        pelementnode[aname]:=''
      else if mtype='xs:integer' then
        pelementnode[aname]:='0'
      else if mtype='xs:float' then
        pelementnode[aname]:='0'
      else if mtype='xs:classid' then
        pelementnode[aname]:=CreateClassID;
    end
    else begin
      if not types.Exists(mtype) then
        raise exception.create('Undefined type '+mtype+' in attribute '+aname);
      xt:=TjanXMLNode2(types[mtype]);
      xfirst:=xt.FirstChild;
      if xfirst=nil then
        raise exception.create('Emtpy type definition '+mtype);
      if xfirst.name='xs:restriction' then begin
        xenum:=xfirst.FirstChild;
        if xenum.name='xs:enumeration' then
          pelementnode[aname]:=xenum['value'];
      end;
    end;
  end;
end;

function TjanXSDParser2.CanDelete(pnode: TjanXMLNode2): boolean;
var
  xp,xe:TjanXMLNode2;
  mname:string;
  minOccurs,maxOccurs,Occurs:integer;
  unbounded:boolean;

begin
  result:=false;
  mname:=pnode.name;
  xe:=GetElement(pnode);
  if xe=nil then
    raise exception.create('Invalid element '+mname);
  minOccurs:=strtointdef(xe['minOccurs'],1);
  maxOccurs:=strtointdef(xe['maxOccurs'],1);
  unbounded:=xe['maxOccurs']='unbounded';
  if minOccurs=0 then
    result:=true
  else begin
    xp:=pnode.parentnode;
    if xp=nil then exit;
    Occurs:=xp.namedChildCount(mname);
    result:=Occurs>minOccurs;
  end;
end;

constructor TjanXSDParser2.Create;
begin
  inherited;
  Types:=TObjectHash.Create;
  Types.OwnsItems:=false;
  Elements:=TList.create;
  Paths:=TStringList.create;
end;

function TjanXSDParser2.CreateElement(pnode: TjanXMLNode2): TjanXMLNode2;
var
  mtype:string;
  xt,xseq, xec:TjanXMLNode2;
  xnc:TjanXMLNode2;
  i,c:integer;
  minOccurs,maxOccurs:integer;
  unbounded:boolean;
begin
  result:=TjanXMLNode2.create;
  result.name:=pnode['name'];
  mtype:=pnode['type'];
  if posstr('xs:',mtype)>0 then exit;
  if not types.Exists(mtype) then
    raise exception.create('Undefined type '+mtype);
  xt:= TjanXMLNode2(types[mtype]);
  AddAttributes(xt,result);
  // any childelements
  xseq:=xt.getChildByName('xs:sequence');
  if xseq<>nil then begin
    c:=xseq.childCount;
    if c>0 then
      for i:=0 to c-1 do begin
        xec:=xseq.childNode[i];
        minOccurs:=strtointdef(xec['minOccurs'],1);
        maxOccurs:=strtointdef(xec['maxOccurs'],1);
        unbounded:=xec['maxOccurs']='unbounded';
        if minOccurs>=1 then begin
          xnc:=CreateElement(xec);
          result.addNode(xnc);
        end;
      end;
  end;
end;

destructor TjanXSDParser2.Destroy;
begin
  Elements.free;
  Types.free;
  Paths.free;
  inherited;
end;


procedure TjanXSDParser2.ValidateNodeTypeAttribute(pnode,
  pattributenode: TjanXMLNode2;pAttributeValue:string);
var
  aname,atype, ause,avalue, adefault:string;
  mbase,rname:string;
  xt,xres,xnc:TjanXMLNode2;
  xsfloat:extended;
  required:boolean;
  i,c:integer;
  guid:TGUID;
begin
  aname:=pattributenode['name'];
  atype:=pattributenode['type'];
  ause:=pattributenode['use'];
  adefault:=pattributenode['default'];
  required:=ause='required';
  avalue:=pAttributeValue;
  if posstr('xs:',atype)>0 then begin
    // basic type
    if atype='xs:string' then begin
      if not pNode.hasAttribute(aname) then
        pNode[aname]:='';
    end
    else if atype='xs:integer' then begin
      if not isinteger(avalue) then
        if FEnforceAttributesOnValidation then
          pNode[aname]:='0'
        else
          raise exception.create(avalue+' is not a valid integer '+aname+' for element '+pnode.name);
    end
    else if atype='xs:float' then begin
      if not isfloat(avalue,xsfloat) then
        if FEnforceAttributesOnValidation then
          pNode[aname]:='0'
        else
          raise exception.create(avalue+' is not a valid float '+aname+' for element '+pnode.name);
    end
    else if atype='xs:date' then begin
      if not isxsdate(avalue) then
        if FEnforceAttributesOnValidation then
          pNode[aname]:=formatdatetime('yyyy-mm-dd',date)
        else
          raise exception.create(avalue+' is not a valid ISO8601 date '+aname+' for element '+pnode.name);
    end
    else if atype='xs:boolean' then begin
      if (avalue<>'0') and (avalue<>'1') then
        if FEnforceAttributesOnValidation then
          pNode[aname]:='0'
        else
          raise exception.create(avalue+' is not a valid boolean '+aname+' for element '+pnode.name);
    end
    else if atype='xs:classid' then begin
      try
        guid:=StringToGUID(avalue);
      except
        if FEnforceAttributesOnValidation then
          pNode[aname]:=CreateClassId
        else
          raise exception.create(avalue+' is not a valid classid '+aname+' for element '+pnode.name);
      end;
    end;
  end
  else begin
    xt:=GetTypeDef(atype);
    if xt=nil then
      raise exception.create('Missing type definition for attribute '+aname+' in element '+pnode.name);
    // only handle restriction
    xres:=xt.getChildByName('xs:restriction');
    if xres=nil then
      raise exception.create('Missing restriction in type definition for attribute '+aname+' in element '+pnode.name);
    mbase:=xres['base'];
    if mbase='' then
      raise exception.create('Missing restriction base type in type definition for attribute '+aname+' in element '+pnode.name);
    if posstr('xs:',mbase)=0 then
      raise exception.create('Invalid restriction base type '+mbase+' in type definition for attribute '+aname+' in element '+pnode.name);
    c:=xres.childCount;
    if c=0 then
      raise exception.create('Empty restriction in type definition for attribute '+aname+' in element '+pnode.name);
    ValidateAttributeRestriction(pnode,aname,avalue,xres);
  end;
end;

procedure TjanXSDParser2.ValidateNodeTypeSequence(pnode, ptypenode,
  psequencenode: TjanXMLNode2);
var
  i,c:integer;
  xnc, childElement, newElement:TjanXMLNode2;
  minOccurs:integer;
  maxOccurs:integer;
  unbounded:boolean;
  childElementCount:integer;
begin
  c:=psequencenode.childCount;
  if c=0 then
    raise exception.create('Empty sequence in type definition '+ptypenode['name']+' for element '+pnode.name);
  for i:=0 to c-1 do begin
    xnc:=psequencenode.childNode[i];
    if xnc.name<>'xs:element' then
      raise exception.create('Schema error: invalid '+xnc.name+' element in xs:sequence of type definition '+ptypenode['name']+' for element '+pnode.name);
    minOccurs:=strtointdef(xnc['minOccurs'],1);
    maxOccurs:=strtointdef(xnc['maxOccurs'],1);
    unbounded:=xnc['maxOccurs']='unbounded';
    if minOccurs>=1 then begin
      childElement:=pnode.getChildByName(xnc['name']);
      if childElement=nil then
        if FEnforceElementsOnValidation then begin
          newElement:=CreateElement(xnc);
          pNode.addNode(newElement);
        end
        else
          raise exception.create('Missing child element '+xnc['name']+' for element '+pnode.name);
    end;
    if not unbounded then begin
      childElementCount:=pnode.namedChildCount(xnc['name']);
      if childElementCount>maxOccurs then
        raise exception.create('Exceeded maximum of '+inttostr(maxOccurs)+' child '+xnc['name']+' for element '+xnc['name']+' for element '+pnode.name);
    end;
  end;
end;

function TjanXSDParser2.GetAttribute(pnode: TjanXMLNode2;pname:string): TjanXMLNode2;
var
  i,c:integer;
  xnc:TjanXMLNode2;
begin
  result:=nil;
  c:=pnode.childCount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xnc:=pnode.childNode[i];
    if (xnc.name='xs:attribute') and (xnc['name']=pname) then begin
      result:=xnc;
      break;
    end;
  end;
end;

procedure TjanXSDParser2.GetChildElements(pNode: TjanXMLNode2;
  pList: TStringList);
var
  i,c:integer;
  xn:TjanXMLNode2;
begin
  c:=pNode.childCount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xn:=pNode.childNode[i];
    if xn.name='xs:element' then
      pList.append(xn['name']);
  end;

end;


function TjanXSDParser2.GetDocumentElement: TjanXMLNode2;
var
  xn:TjanXMLNode2;
begin
  xn:=GetChildByName('xs:element');
  if xn=nil then
    raise exception.create('Missing document element.');
  result:=xn;  
end;

function TjanXSDParser2.GetElement(pnode: TjanXMLNode2): TjanXMLNode2;
var
  nodepath:string;
  tname:string;
  pindex:integer;
begin
  result:=nil;
  nodepath:=GetNodePath(pnode);
  pindex:=Paths.IndexOf(nodepath);
  if pindex=-1 then exit;
  result:=TjanXMLNode2(Paths.objects[pindex]);
end;

function TjanXSDParser2.GetElementType(node: TjanXMLNode2): TjanXMLNode2;
var
  xn:TjanXMLNode2;
begin
  result:=nil;
  xn:=node;
  while xn.parentNode<>nil do begin
    xn:=xn.parentNode;
    if xn.name='xs:complexType' then begin
      result:=xn;
      exit;
    end;
  end;
end;

function TjanXSDParser2.GetNodePath(node: TjanXMLNode2): string;
var
  xn:TjanXMLNode2;
begin
  result:=node.name+'\';
  xn:=node;
  while xn.parentNode<>nil do begin
    xn:=xn.parentNode;
    result:=xn.name+'\'+result;
  end;
end;

function TjanXSDParser2.GetType(pnode: TjanXMLNode2): TjanXMLNode2;
var
  nodepath:string;
  tname:string;
  xe,xt:TjanXMLNode2;
  pindex:integer;
begin
  result:=nil;
  nodepath:=GetNodePath(pnode);
  pindex:=Paths.IndexOf(nodepath);
  if pindex=-1 then exit;
  xe:=TjanXMLNode2(Paths.objects[pindex]);
  tname:=xe['type'];
  if tname='' then exit;
  if not types.Exists(tname) then exit;
  result:=TjanXMLNode2(types[tname]);
end;

function TjanXSDParser2.GetTypeDef(pname: string): TjanXMLNode2;
begin
  if types.Exists(pname) then
    result:=TjanXMLNode2(types[pname])
  else
    result:=nil;  
end;

procedure TjanXSDParser2.getValidAttributes(pnode: TjanXMLNode2;
  pList: TList);
var
  i,c:integer;
  nodepath:string;
  mname,tname,fname:string;
  xe,xt,xfirst:TjanXMLNode2;
  pindex:integer;
begin
  pList.clear;
  nodepath:=GetNodePath(pnode);
  pindex:=Paths.IndexOf(nodepath);
  if pindex=-1 then exit;
  xe:=TjanXMLNode2(Paths.objects[pindex]);
  tname:=xe['type'];
  if tname='' then exit;
  if not types.Exists(tname) then exit;
  xt:=TjanXMLNode2(types[tname]);
  if xt=nil then exit;
  ListSchemaAttributes(xt,pList);
end;

procedure TjanXSDParser2.getValidChildElements(pnode: TjanXMLNode2;
  pList: TList);
var
  i,c:integer;
  nodepath:string;
  mname,tname,fname:string;
  xe,xt,xfirst,xnc:TjanXMLNode2;
  minOccurs,maxOccurs:integer;
  unbounded:boolean;
  pindex:integer;
begin
  pList.clear;
  xe:=GetElement(pnode);
  if xe=nil then
    raise exception.create('Undefined element '+pnode.name);
  tname:=xe['type'];
  if posstr('xs:',tname)<>0 then exit;
  xt:=GetType(pnode);
  if xt=nil then
    raise exception.create('Undefined type for element '+pnode.name);
  ListSchemaElements(xt,pList);
  // remove the nodes with minOccurs=1 and maxOccurs=1 that are allread define
  c:=pList.Count;
  if c=0 then exit;
  for i:=c-1 downto 0 do begin
    xnc:=TjanXMLNode2(pList[i]);
    minOccurs:=strtointdef(xnc['minOccurs'],1);
    maxOccurs:=strtointdef(xnc['maxOccurs'],1);
    unbounded:=xnc['maxOccurs']='unbounded';
    if (maxOccurs=1) and (not unbounded) then begin
      if pnode.getChildByName(xnc['name'])<>nil then
        pList.delete(i);
    end;
  end;
end;

procedure TjanXSDParser2.GetWalkPaths(list: TStringList);
begin
  list.Assign(paths);
end;

function TjanXSDParser2.IsValidEnumeration(pNode: TjanXMLNode2;
  pvalue: string): boolean;
var
  i,c:integer;
  xenum:TjanXMLNode2;
begin
  result:=false;
  c:=pNode.childCount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xenum:=pNode.childNode[i];
    if xenum.name='xs:enumeration' then begin
      if xenum['value']=pvalue then begin
        result:=true;
        break;
      end;
    end;
  end;
end;

procedure TjanXSDParser2.ListElementNames(pNode: TjanXMLNode2;
  pList: TStringList);
var
  i,c:integer;
  xn:TjanXMLNode2;
begin
  c:=pNode.childCount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xn:=pNode.childNode[i];
    if xn.name='xs:element' then
      pList.append(xn['name']);
  end;
end;

procedure TjanXSDParser2.ListElements(pList: TList);
var
  i,c:integer;
  xn:TjanXMLNode2;
begin
  Elements.clear;
  c:=pList.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xn:=TjanXMLNode2(pList[i]);
    if (xn.name='xs:element') then begin
      Elements.Add(xn);
    end;
  end;
end;

procedure TjanXSDParser2.ListEmbeddedElementNames(pNode: TjanXMLNode2;
  pList: TStringList);
begin
//
end;

procedure TjanXSDParser2.ListEnumerations(pnode: TjanXMLNode2;
  plist: TStringList);
var
  i,c:integer;
  xnc:TjanXMLNode2;
begin
  plist.Clear;
  c:=pnode.childCount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xnc:=pnode.childNode[i];
    if xnc.name='xs:enumeration' then
      plist.append(xnc['value']);
  end;
end;

procedure TjanXSDParser2.ListSchemaAttributes(xt: TjanXMLNode2;
  pList: TList);
var
  i,c:integer;
  xa:TjanXMLNode2;
begin
  c:=xt.childCount;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xa:=xt.childNode[i];
    if xa.name='xs:attribute' then
      pList.add(xa);
  end;
end;

procedure TjanXSDParser2.ListSchemaElements(xt: TjanXMLNode2;
  pList: TList);
var
  i,c:integer;
begin
  if xt.name='xs:element' then
    pList.add(xt);
   c:=xt.childCount;
   if c>0 then
     for i:=0 to c-1 do
       ListSchemaElements(xt.childnode[i],pList);
end;

procedure TjanXSDParser2.ListTypeElements(pTypeName: string;
  pList: TStringList);
var
  xtype:TjanXMLNode2;
  xfirst:TjanXMLNode2;
  fname:string;
begin
  if not types.Exists(pTypeName) then exit;
  xtype:=TjanXMLNode2(types[pTypename]);
  xfirst:=xtype.FirstChild;
  if xfirst=nil then exit;
  fname:=xfirst.name;
  if fname='xs:sequence' then
    ListElementNames(xfirst,pList);

end;

procedure TjanXSDParser2.ListTypes(pList: TList);
var
  i,c:integer;
  xn:TjanXMLNode2;
  tname:string;
begin
  Types.Clear;
  c:=pList.count;
  if c=0 then exit;
  for i:=0 to c-1 do begin
    xn:=TjanXMLNode2(pList[i]);
    if (xn.name='xs:complexType') or (xn.name='xs:simpleType') then begin
      tname:=xn['name'];
      if tname<>'' then begin
        if Types.Exists(tname) then
          raise exception.create('Duplicate type declaration '+tname+' in schema');
        Types[tname]:=xn;
      end;
    end;
  end;
end;

procedure TjanXSDParser2.LoadXML(filename: string);
begin
  inherited;
  Prepare;
end;

procedure TjanXSDParser2.Prepare;
var
  list:TList;
  xn:TjanXMLNode2;
begin
  list:=TList.create;
  try
    self.listChildren(list);
    ListTypes(list);
    ListElements(list);
  finally
    list.free;
  end;
  Paths.Clear;
  xn:=self.getChildByName('xs:element');
  if xn=nil then exit;
  TypeWalk(xn,'');
end;

procedure TjanXSDParser2.setXML(const Value: string);
begin
  inherited;
  Prepare;
end;

procedure TjanXSDParser2.TypeWalk(node: TjanXMLNode2;nodepath:string);
var
  mname,mtype:string;
  i,c:integer;
  xnc, xwalk: TjanXMLNode2;
  walkpath:string;
begin
  xwalk:=node;
  mname:=xwalk.name;
  walkpath:=nodepath;
  if mname='xs:element' then begin
    walkpath:=walkpath+xwalk['name']+'\';
    paths.AddObject(walkpath,xwalk);
    mtype:=xwalk['type'];
    if (mtype<>'') and (pos('xs:',mtype)=0) then begin
      if not types.Exists(mtype) then
        raise exception.create('Undefined type '+mtype);
      xwalk:=TjanXMLNode2(types[mtype]);
    end;
  end
  else if mname='xs:extension' then begin
    mtype:=xwalk['base'];
    if (mtype<>'') and (pos('xs:',mtype)=0) then begin
      if not types.Exists(mtype) then
        raise exception.create('Undefined type '+mtype);
      xwalk:=TjanXMLNode2(types[mtype]);
    end;
  end;
  c:=xwalk.childCount;
  if c>0 then
    for i:=0 to c-1 do begin
      xnc:=xwalk.childNode[i];
      TypeWalk(xnc,walkpath);
    end;
end;

procedure TjanXSDParser2.ValidateAttributeRestriction(pnode: TjanXMLNode2;
  pAttributeName, pAttributeValue: string; pRestriction: TjanXMLNode2);
var
  i,c:integer;
  rname:string;
  xnc:TjanXMLNode2;
  valid:boolean;
  EnumMatch:boolean;
  aname, avalue, mbase:string;
  LastOfSequence:boolean;

  procedure _enumeration;
  begin
    if xnc['value']=avalue then begin
      EnumMatch:=true;
    end
    else begin
      if LastOfSequence then
        raise exception.create(avalue+' is not a valid enumeration '+aname+' in element '+pnode.name);
    end;
  end;

  procedure _minInclusive;
  var
    ivalue, itest:integer;
    fvalue, ftest:extended;
    dvalue, dtest:TDate;
    stest:string;
  begin
    stest:=xnc['value'];
    if mbase='xs:integer' then begin
      if not isinteger(stest) then
        raise exception.create('Schema error: '+stest+' is not a valid minInclusive integer');
      itest:=strtoint(stest);
      ivalue:=strtoint(avalue);
      if ivalue<itest then
        raise exception.create('Value '+avalue+' for '+aname+' is out of minInclusive range '+stest+' in element '+pnode.name);
    end
    else if mbase='xs:float' then begin
      if not isfloat(stest) then
        raise exception.create('Schema error: '+stest+' is not a valid minInclusive float');
      ftest:=strtofloat(stest);
      fvalue:=strtofloat(avalue);
      if fvalue<ftest then
        raise exception.create('Value '+avalue+' for '+aname+' is out of minInclusive range '+stest+' in element '+pnode.name);
    end
    else if mbase='xs:date' then begin
      if not isxsdate(stest) then
        raise exception.create('Schema error: '+stest+' is not a valid minInclusive ISO 8601 date');
      dtest:=ISOStringToDate(stest);
      dvalue:=ISOStringToDate(avalue);
      if dvalue<dtest then
        raise exception.create('Value '+avalue+' for '+aname+' is out of minInclusive range '+stest+' in element '+pnode.name);
    end
  end;

  procedure _maxInclusive;
  var
    ivalue, itest:integer;
    fvalue, ftest:extended;
    dvalue, dtest:TDate;
    stest:string;
  begin
    stest:=xnc['value'];
    if mbase='xs:integer' then begin
      if not isinteger(stest) then
        raise exception.create('Schema error: '+stest+' is not a valid maxInclusive integer');
      itest:=strtoint(stest);
      ivalue:=strtoint(avalue);
      if ivalue>itest then
        raise exception.create('Value '+avalue+' for '+aname+' is out of maxInclusive range '+stest+' in element '+pnode.name);
    end
    else if mbase='xs:float' then begin
      if not isfloat(stest) then
        raise exception.create('Schema error: '+stest+' is not a valid maxInclusive float');
      ftest:=strtofloat(stest);
      fvalue:=strtofloat(avalue);
      if fvalue>ftest then
        raise exception.create('Value '+avalue+' for '+aname+' is out of maxInclusive range '+stest+' in element '+pnode.name);
    end
    else if mbase='xs:date' then begin
      if not isxsdate(stest) then
        raise exception.create('Schema error: '+stest+' is not a valid maxInclusive ISO 8601 date');
      dtest:=ISOStringToDate(stest);
      dvalue:=ISOStringToDate(avalue);
      if dvalue>dtest then
        raise exception.create('Value '+avalue+' for '+aname+' is out of maxInclusive range '+stest+' in element '+pnode.name);
    end
  end;


begin
  mbase:=pRestriction['base'];
  avalue:=pAttributeValue;
  aname:=pAttributeName;
  // validate base type
  if mbase='xs:integer' then begin
    if not janstrings.isInteger(avalue) then
      raise exception.create(avalue+' is not a valid integer '+aname+' in element '+pnode.name);
  end
  else if mbase='xs:float' then begin
    if not janstrings.isfloat(avalue) then
      raise exception.create(avalue+' is not a valid float '+aname+' in element '+pnode.name);
  end
  else if mbase='xs:date' then begin
    if not janstrings.isxsdate(avalue) then
      raise exception.create(avalue+' is not a valid ISO8601 date '+aname+' in element '+pnode.name);
  end
  else if mbase='xs:boolean' then begin
    if (avalue<>'0') and (avalue<>'1') then
      raise exception.create(avalue+' is not a boolean '+aname+' in element '+pnode.name);
  end;

  c:=pRestriction.childCount;
  valid:=true;
  EnumMatch:=false;
  for i:=0 to c-1 do begin
    LastOfSequence:=i=(c-1);
    xnc:=pRestriction.childNode[i];
    rname:=xnc.name;
    if rname='xs:enumeration' then _enumeration
    else if rname='xs:minInclusive' then _minInclusive
    else if rname='xs:maxInclusive' then _maxInclusive;
    if EnumMatch then break;
  end;
end;

procedure TjanXSDParser2.ValidateDocument(pxmldocument: TjanXMLParser2);
begin
  ValidateNode(pxmldocument);
end;

procedure TjanXSDParser2.ValidateNode(pnode: TjanXMLNode2);
var
  xe, xt:TjanXMLNode2;
  mtype:string;
  i,c:integer;
begin
  CurrentValidationNode:=pnode;
  xe:=GetElement(pnode);
  if xe=nil then
    raise exception.create('Missing element definition for '+pnode.name);
  mtype:=xe['type'];
  if mtype='' then
    raise exception.create('Missing type attribute for '+pnode.name);
  if posstr('xs:',mtype)=0 then begin
    xt:=GetTypeDef(mtype);
    if xt=nil then
      raise exception.create('Missing type definition for type '+mtype+' for element '+pnode.name);
    // valid typedef
    ValidateNodeType(pnode,xt);
  end
  else
    ValidateNodeTypeBasic(pnode,mtype);
  c:=pnode.childCount;
  if c>0 then
    for i:=0 to c-1 do
      ValidateNode(pnode.childnode[i]);
end;

procedure TjanXSDParser2.ValidateNodeType(pnode, ptypenode: TjanXMLNode2);
var
  i,c:integer;
  xnc:TjanXMLNode2;
begin
  c:=ptypenode.childCount;
  if c=0 then
    raise exception.create('Empty type definition '+ptypenode['name']);
  for i:=0 to c-1 do begin
    xnc:=ptypenode.childNode[i];
    if xnc.name='xs:sequence' then
      ValidateNodeTypeSequence(pnode,ptypenode,xnc)
    else if xnc.name='xs:attribute' then
      ValidateNodeTypeAttribute(pnode,xnc,pnode[xnc['name']]);
  end;
end;

procedure TjanXSDParser2.ValidateNodeTypeBasic(pnode: TjanXMLNode2;
  ptype: string);
begin
//
end;


function TjanXSDParser2.GetCurrentValidationNode: TjanXMLNode2;
begin
  result:=CurrentValidationNode;
end;

function TjanXSDParser2.GetDocumentation(pnode: TjanXMLNode2): string;
var
  xann,xdoc:TjanXMLNode2;
begin
  result:='';
  xann:=pnode.getChildByName('xs:annotation');
  if xann=nil then exit;
  xdoc:=xann.getChildByName('xs:documentation');
  if xdoc<>nil then
    result:=xdoc.text;
end;

function TjanXSDParser2.GetSchemaAttribute(pnode: TjanXMLNode2;
  pAttributeName: string): TjanXMLNode2;
var
  xe,xt,xa:TjanXMLNode2;
  mtype,matype, aname:string;
  i,c:integer;
begin
  result:=nil;
  xe:=GetElement(pnode);
  if xe=nil then
    raise exception.create('Missing element definition for '+pnode.name);
  mtype:=xe['type'];
  if posstr('xs:',mtype)>0 then
    raise exception.create('Missing attribute '+pAttributeName+' definition for element '+pnode.name);
  xt:=GetTypeDef(mtype);
  if xt=nil then
    raise exception.create('Missing Type Definition '+mtype+' for element '+pnode.name);
  c:=xt.childCount;
    // update attributes
  if c=0 then
    raise exception.create('No attributes defined for element '+pnode.name);
  for i:=0 to c-1 do begin
    xa:=xt.childNode[i];
    if xa.name='xs:attribute' then begin
      matype:=xa['type'];
      aname:=xa['name'];
      if aname=pAttributeName then begin
        result:=xa;
        exit;
      end;
    end;
  end;
  raise exception.create('Attribute '+pAttributeName+' is not defined for element '+pnode.name);
end;

function TjanXSDParser2.GetNewPrimaryKey(pDocumentNode,
  pAttributeDef: TjanXMLNode2): string;
var
  list:TList;
  i,c:integer;
  matype, mbase,mtype:string;
  primename:string;
  primevalue:integer;
  minInclusive, maxInclusive, PK:integer;
  xres,xnc:TjanXMLNode2;
  xt:TjanXMLNode2;

  function FindAttributeNameByTypeName(pnode:TjanXMLNode2;ptypename:string):string;
  var
    ia,ca:integer;
    xac:TjanXMLNode2;
  begin
    result:='';
    ca:=pnode.childCount;
    if ca=0 then exit;
    for ia:=0 to ca-1 do begin
      xac:=pnode.childNode[ia];
      if xac.name='xs:attribute' then
        if xac['type']=ptypename then begin
          result:=xac['name'];
          exit;
        end;
    end;

  end;
begin
  matype:=pAttributeDef['name'];
  xres:=pAttributeDef.getChildByName('xs:restriction');
  if xres=nil then
    raise exception.create('Missing xs:restriction in type definition '+pAttributeDef['name']);
  mbase:=xres['base'];
  if mbase<>'xs:integer' then
    raise exception.create('Can not generate primary key for non-integer datatype '+mbase+' in type definition '+pAttributeDef['name']);
  minInclusive:=1;
  maxInclusive:=maxint;
  c:=xres.childCount;
  if c=0 then
    raise exception.create('Missing restrictions in type definition '+pAttributeDef['name']);
  for i:=0 to c-1 do begin
    xnc:=xres.childNode[i];
    if xnc.name='xs:minInclusive' then begin
      if not isinteger(xnc['value']) then
        raise exception.create('Invalid minInclusive integer '+xnc['value']+' in type definition '+pAttributeDef['name']);
      minInclusive:=strtoint(xnc['value']);
    end
    else if xnc.name='xs:maxInclusive' then begin
      if not isinteger(xnc['value']) then
        raise exception.create('Invalid maxInclusive integer '+xnc['value']+' in type definition '+pAttributeDef['name']);
      maxInclusive:=strtoint(xnc['value']);
    end;
  end;
  PK:=minInclusive;
  list:=TList.create;
  result:='';
  try
    pDocumentNode.listChildren(list);
    c:=list.count;
    if c>0 then
      for i:=0 to c-1 do begin
        xnc:=TjanXMLNode2(list[i]);
        xt:=GetType(xnc);
        if xt<>nil then begin
        // get attribute of type matype

          primename:=FindAttributeNameByTypeName(xt,matype);

          if primename<>'' then begin
            primevalue:=strtointdef(xnc[primename],0);
            if primevalue>=PK then
              PK:=primevalue+1;
          end;
        end;
      end;
  finally
    list.free;
  end;
  result:=inttostr(PK);
end;

procedure TjanXSDParser2.EnforceClassIds(pDocumentNode: TjanXMLNode2);
var
  list:TList;
  i,c:integer;
  matype, mbase,mtype:string;
  xres,xcurrent:TjanXMLNode2;
  xt:TjanXMLNode2;

  procedure EnforceAttributes(ptypenode:TjanXMLNode2);
  var
    ia,ca:integer;
    xac:TjanXMLNode2;
    aname:string;
  begin
    ca:=ptypenode.childCount;
    if ca=0 then exit;
    for ia:=0 to ca-1 do begin
      xac:=ptypenode.childNode[ia];
      if xac.name='xs:attribute' then
        if xac['type']='xs:classid' then begin
          aname:=xac['name'];
          if not isclassid(xcurrent[aname]) then
            xcurrent[aname]:=CreateClassId;
        end;
    end;
  end;
begin
  list:=TList.create;
  try
    pDocumentNode.listChildren(list);
    c:=list.count;
    if c>0 then
      for i:=0 to c-1 do begin
        xcurrent:=TjanXMLNode2(list[i]);
        xt:=GetType(xcurrent);
        if xt<>nil then begin
          EnforceAttributes(xt);
        end;
      end;
  finally
    list.free;
  end;
end;

function TjanXSDParser2.isclassid(value: string): boolean;
var
  guid:TGuid;
begin
  result:=false;
  try
    guid:=StringToGUID(value);
    result:=true;
  except
  end;
end;


procedure TjanXSDParser2.SetEnforceAttributesOnValidation(
  const Value: boolean);
begin
  FEnforceAttributesOnValidation := Value;
end;

procedure TjanXSDParser2.SetEnforceElementsOnValidation(
  const Value: boolean);
begin
  FEnforceElementsOnValidation := Value;
end;

end.
