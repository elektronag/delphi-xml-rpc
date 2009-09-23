
{*******************************************************}
{                                                       }
{ XML-RPC Library for Delphi, Kylix and DWPL (DXmlRpc)  }
{                                                       }
{ for Delphi 5 - 7                                      }
{ Release 1.x.x                                         }
{ Copyright (c) 2001-2003 by Team-DelphiXml-Rpc         }
{ e-mail: team-dxmlrpc@dwp42.org                        }
{ www: http://sourceforge.net/projects/delphixml-rpc/   }
{                                                       }
{ The initial developer of the code is                  }
{   Clifford E. Baeseman, codepunk@codepunk.com         }
{                                                       }
{ This file may be distributed and/or modified under    }
{ the terms of the GNU Lesser General Public License    }
{ (LGPL) version 2.1 as published by the Free Software  }
{ Foundation and appearing in the included file         }
{ license.txt.                                          }
{                                                       }
{*******************************************************}
{
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/source/xmlrpctypes.pas,v 1.2 2003-11-20 21:30:14 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  Revision 1.1.1.1  2003/11/19 22:12:23  iwache
  Initial import

  ----------------------------------------------------------------------------
}
unit xmlrpctypes;

interface
uses
  classes,
  contnrs,
  sysutils,
  xmlrpcmime,
  xmlrpccommon;

type
  TArray = class;
  TStruct = class;

  TDataType = (dtDouble, dtInteger,
               dtString, dtBoolean,
               dtDate, dtBase64,
	       dtStruct, dtArray,
	       dtError, dtNone,
	       dtName, dtSingle,dtValue);

  TDataElement = class(TObject)
    FType: TDataType;
    FErrorCode: integer;
    FErrorString: string;
    FString: string;
    FInteger: integer;
    FDate: TDateTime;
    FBoolean: boolean;
    FDouble: Double;
  end;

  TArrayItem = class(TObject)
    FType: TDataType;
    FString: string;
    FStruct: TStruct;
    FArray: TArray;
    FInteger: integer;
    FDouble: double;
    FBoolean: boolean;
    FDate: TDateTime;
    FBase64: string;
    public
      destructor Destroy;override;
    end;

  TArray = class(TObject)
    private
      FList: TObjectList;
   public
     constructor Create;
     destructor Destroy;override;
     procedure AddItem(value: string); overload;
     procedure AddItem(value: integer); overload;
     procedure AddItem(value: boolean); overload;
     procedure AddItem(value: double); overload;
     procedure AddItem(value: TStruct); overload;
     procedure AddItem(value: TArray);overload;
     procedure AddItemDate(value: TDateTime);
     procedure AddItemBase64File(filename: string);
     procedure AddItemBase64String(value: string);
     procedure AddItemBase64Data(value:string);
     function Count: integer;
     function IsString(index: integer):boolean;
     function IsInteger(index: integer):boolean;
     function IsBoolean(index: integer):boolean;
     function IsDate(index: integer):boolean;
     function IsDouble(index: integer):boolean;
     function IsStruct(index:integer):boolean;
     function IsArray(index:integer):boolean;
     function IsBase64(index:integer):boolean;
     function GetString(index: integer): string;
     function GetInteger(index: integer): integer;
     function GetBoolean(index: integer): boolean;
     function GetDate(index: integer): TDateTime;
     function GetDouble(index: integer): double;
     function GetStruct(index: integer): TStruct;
     function GetArray(index: integer):TArray;
     function GetBase64Stream(index: integer):TMemoryStream;
     function GetBase64String(index:integer):string;
     function GetAsXML:string;
     procedure LoadRawData(dt: TDataType; value: string);
     procedure Delete(index: integer);
     procedure SetString(index: integer; value: string);
     procedure SetBoolean(index: integer; value: boolean);
     procedure SetInteger(index: integer; value: integer);
     procedure SetBase64String(index: integer; value: string);
     procedure SetBase64File(index: integer; FileName: string);
     procedure SetBase64Data(index: integer; value: TMemoryStream);
     procedure SetDouble(index: integer; value: double);
     procedure SetDate(index: integer; value: TDateTime);
     procedure SetStruct(index: integer; value: TStruct);
     procedure SetArray(index: integer; value: TArray);
     procedure Clear;
    end;


  TStructItem = class(TObject)
     FName: string;
     FType: TDataType;
     FStruct: TStruct;
     FArray: TArray;
     FString: string;
     FInteger: integer;
     FDouble: double;
     FBoolean: boolean;
     FDate: TDateTime;
     FBase64: string;
     public
     destructor Destroy;override;
    end;

  TStruct = class(TObject)
  private
    FItemList: TStringList;
  public
    constructor Create;
    destructor Destroy;override;
    procedure AddItem(key: string; value: integer); overload;
    procedure AddItem(key: string; value: string); overload;
    procedure AddItem(key: string; value: double); overload;
    procedure AddItem(key: string; value: boolean); overload;
    procedure AddItem(key: string; value: TArray);overload;
    procedure AddItem(key: string; value: TStruct);overload;
    procedure AddItemDate(key: string; value: TDateTime);
    procedure AddItemBase64String(key: string; value: string);
    procedure AddItemBase64File(key: string; filename: string);
    procedure AddItemBase64Data(key: string; value: string);
    function GetKeyList: TStringList;
    function HasKey(key: string): boolean;
    function GetInteger(key: string): integer; overload;
    function GetDouble(key: string): double; overload;
    function GetString(key: string): string; overload;
    function GetBoolean(key: string): boolean; overload;
    function GetArray(key: string): TArray; overload;
    function GetStruct(key: string):TStruct; overload;
    function GetDate(key: string): TDateTime; overload;
    function GetBase64String(key: string): string; overload;
    function GetBase64Stream(key: string): TMemoryStream; overload;
    function GetInteger(index: integer): integer; overload;
    function GetDouble(index: integer): double; overload;
    function GetString(index: integer): string; overload;
    function GetBoolean(index: integer): boolean; overload;
    function GetArray(index: integer): TArray; overload;
    function GetStruct(index: integer):TStruct; overload;
    function GetDate(index: integer): TDateTime; overload;
    function GetBase64String(index: integer): string; overload;
    function GetBase64Stream(index: integer): TMemoryStream; overload;
    function IsArray(key: string):boolean; overload;
    function IsStruct(key: string):boolean; overload;
    function IsString(key: string):boolean; overload;
    function IsInteger(key: string):boolean; overload;
    function IsDouble(key: string):boolean; overload;
    function IsBoolean(key: string):boolean; overload;
    function IsDate(key: string):boolean; overload;
    function IsBase64(key: string):boolean; overload;
    function IsArray(index: integer):boolean; overload;
    function IsStruct(index:integer):boolean; overload;
    function IsString(index: integer):boolean; overload;
    function IsInteger(index: integer):boolean; overload;
    function IsDouble(index: integer):boolean; overload;
    function IsBoolean(index: integer):boolean; overload;
    function IsDate(index: integer):boolean; overload;
    function IsBase64(index: integer):boolean; overload;
    procedure Delete(key: string);
    procedure SetString(key: string; value: string);
    procedure SetBoolean(key: string; value: boolean);
    procedure SetInteger(key: string; value: integer);
    procedure SetBase64String(key: string; value: string);
    procedure SetBase64File(key: string; FileName: string);
    procedure SetBase64Data(key: string; value: TMemoryStream);
    procedure SetDouble(key: string; value: double);
    procedure SetDate(key: string; value: TDateTime);
    procedure SetStruct(key: string; value: TStruct);
    procedure SetArray(key: string; value: TArray);
    procedure Clear;
    procedure LoadRawData(dt: TDataType; key: string; value: string);
    function Count: integer;
    function GetAsXML: string;
   end;

  TFunctionItem = class(TObject)
    FType: TDataType;
    FString: string;
    FStruct: TStruct;
    FArray: TArray;
    FInteger: integer;
    FDouble: double;
    FBoolean: boolean;
    FDate: TDateTime;
    FBase64: string;
  public
    destructor Destroy;override;
  end;

  TFunction = class(TObject)
    private
      FList: TObjectList;
      FMethod: string;
      FErrorCode: integer;
      FErrorMessage: string;

    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      property ObjectMethod: string read FMethod write FMethod;
      procedure AddParam(value: integer);overload;
      procedure AddParam(value: string);overload;
      procedure AddParam(value: double);overload;
      procedure AddParam(value: boolean);overload;
      procedure AddParam(value: TStruct);overload;
      procedure AddParam(value: TArray);overload;
      procedure AddParamDate(value: TDateTime);
      procedure AddParamBase64String(value: string);
      procedure AddParamBase64File(filename: string);
      procedure AddParamBase64Crypto(key,value:string);
      procedure AddError(code: integer; msg: string);
      function GetRequestXML: string;
      function GetResponseXML:string;
      function GetErrorXML: string;
    end;

  TResult = class(TObject)
    private
      FStruct: TStruct;
      FArray: TArray;
      FType: TDataType;
      FErrorCode: integer;
      FErrorString: string;
      FInteger: integer;
      FString: string;
      FDouble: double;
      FDate: TDateTime;
      FBoolean: boolean;
      FBase64: string;
    public
      destructor Destroy;override;
      procedure Clear;
      procedure SetError;
      procedure AddItem(value: string);overload;
      procedure AddItem(value: integer);overload;
      procedure AddItem(value: double);overload;
      procedure AddItem(value: boolean);overload;
      procedure AddItem(value: TStruct);overload;
      procedure AddItem(value: TArray);overload;
      procedure AddItemDate(value: TDateTime);overload;
      procedure AddItemBase64Data(value: string);
      procedure AddErrorCode(value: integer);
      procedure AddErrorString(value: string);
      function GetBase64String: string;
      function GetBase64Raw: string;
      function GetBase64Stream:TMemoryStream;
      procedure PutBase64ToFile(filename: string);
      function GetString: string;
      function GetInteger: integer;
      function GetDouble: double;
      function GetDate: TDateTime;
      function GetBoolean: boolean;
      function GetStruct: TStruct;
      function GetArray: TArray;
      function GetErrorCode: integer;
      function GetErrorString: string;
      function IsError: boolean;
      function IsInteger: boolean;
      function IsString: boolean;
      function IsDouble: boolean;
      function IsDate: boolean;
      function IsBoolean: boolean;
      function IsStruct: boolean;
      function IsArray: boolean;
      function IsBase64: boolean;
    end;

    TParameter = TResult;
    TReturn = TFunction;

implementation


{------------------------------------------------------------------------------}
procedure TStruct.AddItem(key, value: string);
var
 titem: TStructItem;
begin
  titem := TStructItem.Create;
  titem.FType := dtString;
  titem.FString := EncodeEntities(value);
  FItemList.AddObject(key,titem);
end;

{------------------------------------------------------------------------------}
procedure TStruct.AddItem(key: string; value: double);
var
 titem: TStructItem;
begin
  titem := TStructItem.Create;
  titem.FType := dtDouble;
  titem.FDouble := value;
  FItemList.AddObject(key,titem);
end;

{------------------------------------------------------------------------------}
procedure TStruct.AddItem(key: string; value: integer);
var
 titem: TStructItem;
begin
  titem := TStructItem.Create;
  titem.FType := dtInteger;
  titem.FInteger := value;
  FItemList.AddObject(key,titem);
end;

{------------------------------------------------------------------------------}
procedure TStruct.AddItem(key: string; value: TArray);
var
 titem: TStructItem;
begin
  titem := TStructItem.Create;
  titem.FType := dtArray;
  titem.FArray := value;
  FItemList.AddObject(key,titem);
end;

{------------------------------------------------------------------------------}
procedure TStruct.AddItem(key: string; value: Boolean);
var
 titem: TStructItem;
begin
  titem := TStructItem.Create;
  titem.FType := dtBoolean;
  titem.FBoolean := value;
  FItemList.AddObject(key,titem);
end;

{------------------------------------------------------------------------------}
procedure TStruct.AddItemBase64File(key, filename: string);
var
 tf: TFileStream;
 ti: TStructItem;
begin
  ti := TStructItem.Create;
  ti.FType := dtBase64;
  tf := TFileStream.Create(filename,fmOpenRead);
  ti.FBase64 :=  MimeEncodeStringNoCRLF(StreamToString(tf));
  FItemList.AddObject(key,ti);
  FreeAndNil(tf);
end;

{------------------------------------------------------------------------------}
procedure TStruct.AddItemBase64String(key, value: string);
var
 ti: TStructItem;
begin
  ti := TStructItem.Create;
  ti.FType := dtBase64;
  ti.FBase64 := MimeEncodeStringNoCRLF(value);
  FItemList.AddObject(key,ti);
end;

{------------------------------------------------------------------------------}
procedure TStruct.AddItemDate(key: string; value: TDateTime);
var
 titem: TStructItem;
begin
  titem := TStructItem.Create;
  titem.FType := dtDate;
  titem.FDate := value;
  FItemList.AddObject(key,titem);
end;

{------------------------------------------------------------------------------}
function TStruct.Count: integer;
begin
  result := FItemList.Count;
end;

{------------------------------------------------------------------------------}
constructor TStruct.Create;
begin
  FItemList := TStringList.Create;
end;

{------------------------------------------------------------------------------}
function TStruct.GetBase64Stream(key: string): TMemoryStream;
var
 mstream: TMemoryStream;
 stemp: string;
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.GetBase64Stream(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[i]).FType = dtBase64 then
    begin
      mstream := TMemoryStream.Create;
      stemp := MimeDecodeString(TStructItem(FItemList.Objects[i]).FBase64);
      StringToStream(stemp, mstream);
      result := mstream;
    end
  else
    raise Exception.Create('TStruct.GetBase64Stream(' + key + ') - Item is not a base64 value');
end;

{------------------------------------------------------------------------------}
function TStruct.GetBase64Stream(index: integer): TMemoryStream;
var
 mstream: TMemoryStream;
 stemp: string;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtBase64 then
    begin
      mstream := TMemoryStream.Create;
      stemp := MimeDecodeString(TStructItem(FItemList.Objects[index]).FBase64);
      StringToStream(stemp, mstream);
      result := mstream;
    end
  else
    raise Exception.Create('TStruct.GetBase64Stream(' + IntToStr(index) + ') - Item is not a base64 value');
end;

{------------------------------------------------------------------------------}
function TStruct.GetBase64String(index: integer): string;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtBase64 then
    result := MimeDecodeString(TStructItem(FItemList.Objects[index]).FBase64)
  else
    raise Exception.Create('TStruct.GetBase64String(' + IntToStr(index) + ') - Item is not a base64 value');
end;

{------------------------------------------------------------------------------}
function TStruct.GetBase64String(key: string): string;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.GetBase64String(' + key + ') - key not found');

  if TStructItem(FItemList.Objects[i]).FType = dtBase64 then
     result := MimeDecodeString(TStructItem(FItemList.Objects[i]).FBase64)
  else
    raise Exception.Create('TStruct.GetBase64String(' + key + ') - Item is not a base64 value');
end;

{------------------------------------------------------------------------------}
function TStruct.GetBoolean(key: string): boolean;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.GetBoolean(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[i]).FType = dtBoolean then
    result := TStructItem(FItemList.Objects[i]).FBoolean
  else
    raise Exception.Create('TStruct.GetBoolean(' + key + ') - Item is not a boolean');
end;

{------------------------------------------------------------------------------}
function TStruct.GetBoolean(index: integer): boolean;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtBoolean then
    result := TStructItem(FItemList.Objects[index]).FBoolean
  else
    raise Exception.Create('TStruct.GetBoolean(' + IntToStr(index) + ') - Item is not a boolean');
end;

{------------------------------------------------------------------------------}
function TStruct.GetDate(index: integer): TDateTime;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtDate then
    result := TStructItem(FItemList.Objects[index]).FDate
  else
    raise Exception.Create('TStruct.GetDate(' + IntToStr(index) + ') - Item is not a date');
end;

{------------------------------------------------------------------------------}
function TStruct.GetDate(key: string): TDateTime;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.GetDate(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[i]).FType = dtDate then
    result := TStructItem(FItemList.Objects[i]).FDate
  else
    raise Exception.Create('TStruct.GetDate(' + key + ') - Item is not a date');
end;

{------------------------------------------------------------------------------}
function TStruct.GetDouble(key: string): double;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.GetDouble(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[i]).FType = dtDouble then
    result := TStructItem(FItemList.Objects[i]).FDouble
  else
    raise Exception.Create('TStruct.GetDouble(' + key + ') - Item is not a double');
end;

{------------------------------------------------------------------------------}
function TStruct.GetDouble(index: integer): double;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtDouble then
    result := TStructItem(FItemList.Objects[index]).FDouble
  else
    raise Exception.Create('TStruct.GetDouble(' + IntToStr(index) + ') - Item is not a double');
end;

{------------------------------------------------------------------------------}
function TStruct.GetInteger(key: string): integer;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.GetInteger(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[i]).FType = dtInteger then
    result := TStructItem(FItemList.Objects[i]).FInteger
  else
    raise Exception.Create('TStruct.GetInteger(' + key + ') - Item is not a integer');
end;

{------------------------------------------------------------------------------}
function TStruct.GetInteger(index: integer): integer;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtInteger then
    result := TStructItem(FItemList.Objects[index]).FInteger
  else
    raise Exception.Create('TStruct.GetInteger(' + IntToStr(index) + ') - Item is not a integer');
end;

{------------------------------------------------------------------------------}
function TStruct.GetKeyList: TStringList;
begin
  result := FItemList;
end;

{------------------------------------------------------------------------------}
function TStruct.GetArray(index: integer): TArray;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtArray then
    result := TStructItem(FItemList.Objects[index]).FArray
  else
    raise Exception.Create('TStruct.GetArray(' + IntToStr(index) + ') - Item is not an array');
end;

{------------------------------------------------------------------------------}
function TStruct.GetArray(key: string): TArray;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.GetArray(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[i]).FType = dtArray then
    result := TStructItem(FItemList.Objects[i]).FArray
  else
    raise Exception.Create('TStruct.GetArray(' + key + ') - Item is not an array');
end;

{------------------------------------------------------------------------------}
function TStruct.GetString(key: string): string;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.GetString(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[i]).FType = dtString then
    result := TStructItem(FItemList.Objects[i]).FString
  else
    raise Exception.Create('TStruct.GetString(' + key + ') - Item is not a string');
end;

{------------------------------------------------------------------------------}
function TStruct.GetString(index: integer): string;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtString then
    result := TStructItem(FItemList.Objects[index]).FString
  else
    raise Exception.Create('TStruct.GetString(' + IntToStr(index) + ') - Item is not a string');
end;

{------------------------------------------------------------------------------}
function TStruct.HasKey(key: string): boolean;
begin
  if FItemList.IndexOf(key) <> -1 then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsBase64(key: string): boolean;
begin
  if TStructItem(FItemList.Objects[FItemList.IndexOf(key)]).FType = dtBase64 then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsBase64(index: integer): boolean;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtBase64 then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsBoolean(index: integer): boolean;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtBoolean then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsBoolean(key: string): boolean;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.IsBoolean(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[i]).FType = dtBoolean then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsDate(index: integer): boolean;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtDate then
    result := true
   else
     result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsDate(key: string): boolean;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.IsDate(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[FItemList.IndexOf(key)]).FType = dtDate then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsDouble(key: string): boolean;
begin
  if TStructItem(FItemList.Objects[FItemList.IndexOf(key)]).FType = dtDouble then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsDouble(index: integer): boolean;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtDouble then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsInteger(index: integer): boolean;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtInteger then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsInteger(key: string): boolean;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.IsInteger(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[FItemList.IndexOf(key)]).FType = dtInteger then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsArray(key: string): boolean;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.IsArray(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[FItemList.IndexOf(key)]).FType = dtArray then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsArray(index: integer): boolean;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtArray then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsString(index: integer): boolean;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtString then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsString(key: string): boolean;
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.IsString(' + key + ') - key not found');
  if TStructItem(FItemList.Objects[FItemList.IndexOf(key)]).FType = dtString then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
procedure TStruct.AddItem(key: string; value: TStruct);
var
 titem: TStructItem;
begin
  titem := TStructItem.Create;
  titem.FType := dtStruct;
  titem.FStruct := value;
  FItemList.AddObject(key,titem);
end;

{------------------------------------------------------------------------------}
function TStruct.GetAsXML: string;
var
 index: integer;
 tt : TDataType;
 st: TStrings;
begin
  st := TStringList.Create;
  st.Add('<value>');
  st.Add('  <struct>');
  for index := 0 to FItemList.Count - 1 do
    begin
      st.Add('    <member>');
      tt := TStructItem(FItemList.Objects[index]).FType;
      case tt of
        dtString:
	  begin
	    st.Add('      <name>' +
	           FItemList[index]  +
		   '</name>');
	   st.Add('      <value><string>' +
	          TStructItem(FItemList.Objects[index]).FString +
		  '</string></value>');
	   end;
	 dtInteger:
	   begin
	     st.Add('      <name>' +
	            FItemList[index]  +
		    '</name>');
	     st.Add('      <value><int>' +
	            IntToStr(TStructItem(FItemList.Objects[index]).FInteger) +
	            '</int></value>');
	   end;
	 dtDouble:
	   begin
	     st.Add('      <name>' +
	            FItemList[index]  +
		    '</name>');
	     st.Add('      <value><double>' +
	            FloatToStr(TStructItem(FItemList.Objects[index]).FDouble) +
		    '</double></value>');
	   end;
	 dtBase64:
	   begin
	     st.Add('      <name>' +
                     FItemList[index]  +
                    '</name>');
             st.Add('      <value><base64>' +
                     TStructItem(FItemList.Objects[index]).FBase64 +
		     '</base64></value>');
           end;
        dtDate:
           begin
            st.Add('      <name>' +
                   FItemList[index]  +
                   '</name>');
            st.Add('      <value><dateTime.iso8601>' +
                         DateTimeToISO(TStructItem(FItemList.Objects[index]).FDate) +
                         '</dateTime.iso8601></value>');
           end;
        dtBoolean:
           begin
            st.Add('      <name>' +
                   FItemList[index]  +
                   '</name>');
            if TStructItem(FItemList.Objects[index]).FBoolean then
              st.Add('      <value><boolean>1</boolean></value>')
            else
              st.Add('      <value><boolean>0</boolean></value>');
           end;
        dtStruct:
           begin
             st.Add('      <name>' +
                   FItemList[index]  +
                   '</name>');  //Otherwise adds space to name - LEE 1/10/2003
             st.Add(TStructItem(FItemList.Objects[index]).FStruct.GetAsXML);
           end;
        dtArray:
           begin
             st.Add('      <name>' +
                   FItemList[index]  +
                   '</name>');
             st.Add(TStructItem(FItemList.Objects[index]).FArray.GetAsXML);
           end;
      end;
		 st.Add('    </member>');
		end;
    st.Add('  </struct>');
    st.Add('</value>');
    // result := st.GetText; // GetText producece MemLeak. 14.8.2003 / mko
    result := st.Text;
    FreeAndNil(st);
end;

{------------------------------------------------------------------------------}
function TStruct.GetStruct(index: integer): TStruct;
begin
  if TStructItem(FItemList.Objects[index]).FType = dtStruct then
    result := TStructItem(FItemList.Objects[index]).FStruct
  else
    raise Exception.Create('TStruct.GetStruct(' + IntToStr(index) + ') - Item is not a struct type');
end;

{------------------------------------------------------------------------------}
function TStruct.GetStruct(key: string): TStruct;
begin
  if TStructItem(FItemList.Objects[FItemList.IndexOf(key)]).FType = dtStruct then
    result := TStructItem(FItemList.Objects[FItemList.IndexOf(key)]).FStruct
  else
    raise Exception.Create('TStruct.GetStruct(' + key + ') - Item is not a struct type');
end;

{------------------------------------------------------------------------------}
function TStruct.IsStruct(index: integer): boolean;
begin                     //Check object not string - LEE 1/10/2003
  if TStructItem(FItemList.objects[index]).FType = dtStruct then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TStruct.IsStruct(key: string): boolean;
begin
  if TStructItem(FItemList.Objects[FItemList.IndexOf(key)]).FType = dtStruct then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
procedure TStruct.AddItemBase64Data(key, value: string);
var
 ti: TStructItem;
begin
  ti := TStructItem.Create;
  ti.FType := dtBase64;
  ti.FBase64 := value;
  FItemList.AddObject(key,ti);
end;

{------------------------------------------------------------------------------}
destructor TStruct.Destroy;
var
 index: integer;
begin
    inherited;
   for index := 0 to FItemList.Count -1 do
    begin
     TStructItem(FItemList.Objects[index]).Free;
    end;
    FreeAndNil(FItemList);

end;

{------------------------------------------------------------------------------}
procedure TStruct.LoadRawData(dt: TDataType; key: string; value: string);
var
 titem: TStructItem;
begin
  case dt of
    dtString:
      begin
        titem := TStructItem.Create;
        titem.FType := dtString;
        titem.FString := value;
        FItemList.AddObject(key,titem);
      end;
    dtInteger:
      begin
        titem := TStructItem.Create;
        titem.FType := dtInteger;
        titem.FInteger := StrToInt(value);
        FItemList.AddObject(key,titem);
      end;
    dtBoolean:
      begin
        titem := TStructItem.Create;
        titem.FType := dtBoolean;
        if value = '0' then
          titem.FBoolean := false
        else
          titem.FBoolean := true;
        FItemList.AddObject(key,titem);
      end;
    dtDouble:
      begin
        titem := TStructItem.Create;
        titem.FType := dtDouble;
        titem.FDouble := StrToFloat(value);
        FItemList.AddObject(key,titem);
      end;
    dtDate:
      begin
        titem := TStructItem.Create;
        titem.FType := dtDate;
        titem.FDate := ISOToDateTime(value);
        FItemList.AddObject(key,titem);
      end;
    dtBase64:
      begin
        titem := TStructItem.Create;
        titem.FType := dtBase64;
        titem.FBase64 := value;
        FItemList.AddObject(key,titem);
      end;
  end;

end;

{------------------------------------------------------------------------------}
procedure TArray.AddItem(value: double);
var
 ti: TArrayItem;
begin
  ti := TArrayItem.Create;
  ti.FType := dtDouble;
  ti.FDouble := value;
  FList.Add(ti);
end;

{------------------------------------------------------------------------------}
procedure TArray.AddItem(value: boolean);
var
 ti: TArrayItem;
begin
  ti := TArrayItem.Create;
  ti.FType := dtBoolean;
  ti.FBoolean := value;
  FList.Add(ti);
end;

{------------------------------------------------------------------------------}
procedure TArray.AddItem(value: TArray);
var
 ti: TArrayItem;
begin
  ti := TArrayItem.Create;
  ti.FType := dtArray;
  ti.FArray := value;
  FList.Add(ti);
end;

{------------------------------------------------------------------------------}
procedure TArray.AddItem(value: TStruct);
var
 ti: TArrayItem;
begin
  ti := TArrayItem.Create;
  ti.FType := dtStruct;
  ti.FStruct := value;
  FList.Add(ti);
end;

{------------------------------------------------------------------------------}
procedure TArray.AddItem(value: string);
var
 ti: TArrayItem;
begin
  ti := TArrayItem.Create;
  ti.FType := dtString;
  ti.FString := value;
  FList.Add(ti);
end;

{------------------------------------------------------------------------------}
procedure TArray.AddItem(value: integer);
var
 ti: TArrayItem;
begin
  ti := TArrayItem.Create;
  ti.FType := dtInteger;
  ti.FInteger := value;
  FList.Add(ti);
end;

{------------------------------------------------------------------------------}
procedure TArray.AddItemBase64Data(value: string);
var
 ti: TArrayItem;
begin
  ti := TArrayItem.Create;
  ti.FType := dtBase64;
  ti.FBase64 := value;
  FList.Add(ti);
end;

{------------------------------------------------------------------------------}
procedure TArray.AddItemBase64File(filename: string);
var
 tf: TFileStream;
 ti: TArrayItem;
begin
  ti := TArrayItem.Create;
  ti.FType := dtBase64;
  tf := TFileStream.Create(filename,fmOpenRead);
  ti.FBase64 :=  MimeEncodeStringNoCRLF(StreamToString(tf));
  FList.Add(ti);
  FreeAndNil(tf);
end;

{------------------------------------------------------------------------------}
procedure TArray.AddItemBase64String(value: string);
var
 ti: TArrayItem;
begin
  ti := TArrayItem.Create;
  ti.FType := dtBase64;
  ti.FBase64 := MimeEncodeStringNoCRLF(value);
  FList.Add(ti);
end;

{------------------------------------------------------------------------------}
procedure TArray.AddItemDate(value: TDateTime);
var
 ti: TArrayItem;
begin
  ti := TArrayItem.Create;
  ti.FType := dtDate;
  ti.FDate := value;
  FList.Add(ti);
end;

{------------------------------------------------------------------------------}
procedure TArray.Clear;
begin
 FList.Clear;
end;

function TArray.Count: integer;
begin
  result := FList.Count;
end;

{------------------------------------------------------------------------------}
constructor TArray.Create;
begin
  FList := TObjectList.Create;
end;

{------------------------------------------------------------------------------}
procedure TArray.Delete(index: integer);
begin

end;

destructor TArray.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

{------------------------------------------------------------------------------}
function TArray.GetArray(index: integer): TArray;
begin
  if TArrayItem(FList[index]).FType = dtArray then
    result := TArrayItem(FList[index]).FArray
  else
    raise exception.Create('TArray.GetArray(' + IntToStr(index) + ') - Item is not an array type');
end;

{------------------------------------------------------------------------------}
function TArray.GetAsXML: string;
var
 index: integer;
 tt : TDataType;
 st: TStrings;
begin
  st := TStringList.Create;
  st.Add('<value>');
  st.Add('  <array>');
  st.Add('    <data>');
  for index := 0 to FList.Count - 1 do
    begin
      tt := TArrayItem(FList[index]).FType;
      case tt of
        dtString: st.Add('      <value><string>' +
                         TArrayItem(FList[index]).FString +
                         '</string></value>');
        dtInteger: st.Add('      <value><int>' +
                         IntToStr(TArrayItem(FList[index]).FInteger) +
                         '</int></value>');
        dtDouble: st.Add('      <value><double>' +
                         FloatToStr(TArrayItem(FList[index]).FDouble) +
                         '</double></value>');
        dtBase64: st.Add('      <value><base64>' +
                         TArrayItem(FList[index]).FBase64 +
                         '      </base64></value>');
        dtDate: st.Add('      <value><dateTime.iso8601>' +
                         DateTimeToISO(TArrayItem(FList[index]).FDate) +
                         '</dateTime.iso8601></value>');
        dtBoolean:
          if TArrayItem(FList[index]).FBoolean then
            st.Add('      <value><boolean>1</boolean></value>')
          else
            st.Add('      <value><boolean>0</boolean></value>');

        dtStruct: st.Add(TArrayItem(FList[index]).FStruct.GetAsXML);
        dtArray: st.Add(TArrayItem(FList[index]).FArray.GetAsXML);
      end;
    end;
     st.Add('  </data>');
     st.Add('</array>');
     st.Add('</value>');
     // result := st.GetText; // GetText producece MemLeak. 14.8.2003 / mko
    result := st.Text;
    FreeAndNil(st);
end;

{------------------------------------------------------------------------------}
function TArray.GetBase64Stream(index: integer): TMemoryStream;
var
 mstream: TMemoryStream;
 stemp: string;
begin
 if TArrayItem(FList[index]).FType = dtBase64 then
   begin
    mstream := TMemoryStream.Create;
    stemp := MimeDecodeString(TArrayItem(FList[index]).FBase64);
    StringToStream(stemp, mstream);
    result := mstream;
   end
  else
    raise Exception.Create('TArray.GetBase64Stream(' + IntToStr(index) + ') - Item is not a base64 type');
end;

{------------------------------------------------------------------------------}
function TArray.GetBase64String(index: integer): string;
begin
  if TArrayItem(FList[index]).FType = dtBase64 then
    result := MimeDecodeString(TArrayItem(Flist[index]).Fbase64)
  else
    raise Exception.Create('TArray.GetBase64String(' + IntToStr(index) + ') - Item is not a base64 type');
end;

{------------------------------------------------------------------------------}
function TArray.GetBoolean(index: integer): boolean;
begin
  if TArrayItem(FList[index]).FType = dtBoolean then
    result := TArrayItem(FList[index]).FBoolean
  else
    raise exception.Create('TArray.GetBoolean(' + IntToStr(index) + ') - Item is not a boolean type');
end;

{------------------------------------------------------------------------------}
function TArray.GetDate(index: integer): TDateTime;
begin
  if TArrayItem(FList[index]).FType = dtDate then
    result := TArrayItem(FList[index]).FDate
  else
    raise exception.Create('TArray.GetDate(' + IntToStr(index) + ') - Item is not a date type');
end;

{------------------------------------------------------------------------------}
function TArray.GetDouble(index: integer): double;
begin
  if TArrayItem(FList[index]).FType = dtDouble then
    result := TArrayItem(FList[index]).FDouble
  else
    raise exception.Create('TArray.GetDouble(' + IntToStr(index) + ') - Item is not a double type');
end;

{------------------------------------------------------------------------------}
function TArray.GetInteger(index: integer): integer;
begin
  if TArrayItem(FList[index]).FType = dtInteger then
    result := TArrayItem(FList[index]).FInteger
  else
    raise exception.Create('TArray.GetInteger(' + IntToStr(index) + ') - Item is not a integer type');
end;

{------------------------------------------------------------------------------}
function TArray.GetString(index: integer): string;
begin
  if TArrayItem(FList[index]).FType = dtString then
    result := TArrayItem(FList[index]).FString
  else
    raise exception.Create('TArray.GetString(' + IntToStr(index) + ') - Item is not a string type');
end;

{------------------------------------------------------------------------------}
function TArray.GetStruct(index: integer): TStruct;
begin
  if TArrayItem(FList[index]).FType = dtStruct then
    result := TArrayItem(FList[index]).FStruct
  else
    raise exception.Create('item is not a struct');
end;

{------------------------------------------------------------------------------}
function TArray.IsArray(index: integer): boolean;
begin
  if TArrayItem(FList[index]).FType = dtArray then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TArray.IsBase64(index: integer): boolean;
begin
  if TArrayItem(FList[index]).FType = dtBase64 then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TArray.IsBoolean(index: integer): boolean;
begin
  if TArrayItem(FList[index]).FType = dtBoolean then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TArray.IsDate(index: integer): boolean;
begin
   if TArrayItem(FList[index]).FType = dtDate then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TArray.IsDouble(index: integer): boolean;
begin
  if TArrayItem(FList[index]).FType = dtDouble then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TArray.IsInteger(index: integer): boolean;
begin
  if TArrayItem(FList[index]).FType = dtInteger then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TArray.IsString(index: integer): boolean;
begin
   if TArrayItem(FList[index]).FType = dtString then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TArray.IsStruct(index: integer): boolean;
begin
  if TArrayItem(FList[index]).FType = dtStruct then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
procedure TArray.LoadRawData(dt: TDataType; value: string);
var
 ti : TArrayItem;
begin
 case dt of
  dtString:
    begin
      ti := TArrayItem.Create;
      ti.FType := dtString;
      ti.FString := value;
      FList.Add(ti);
    end;
  dtInteger:
    begin
      ti := TArrayItem.Create;
      ti.FType := dtInteger;
      ti.FInteger := StrToInt(value);
      FList.Add(ti);
    end;
  dtDouble:
    begin
      ti := TArrayItem.Create;
      ti.FType := dtDouble;
      ti.FDouble := StrToFloat(value);
      FList.Add(ti);
    end;
  dtBoolean:
    begin
      ti := TArrayItem.Create;
      ti.FType := dtBoolean;
      if (value = '0') then
        ti.FBoolean := false
      else
        ti.FBoolean := true;
      FList.Add(ti);
    end;
  dtDate:
    begin
      ti := TArrayItem.Create;
      ti.FType := dtDate;
      ti.FDate := ISOToDateTime(value);
      FList.Add(ti);
    end;
  dtBase64:
    begin
      ti := TArrayItem.Create;
      ti.FType := dtBase64;
      ti.FBase64 := value;
      FList.Add(ti);
    end;
 end;

end;

{------------------------------------------------------------------------------}
procedure TFunction.AddParam(value: double);
var
 fi: TFunctionItem;
begin
  fi := TFunctionItem.Create;
  fi.FDouble := value;
  fi.FType := dtDouble;
  FList.Add(fi);

end;

{------------------------------------------------------------------------------}
procedure TFunction.AddParam(value: string);
var
 fi: TFunctionItem;
begin
  fi := TFunctionItem.Create;
  fi.FString := EncodeEntities(value);
  fi.FType := dtString;
  FList.Add(fi);

end;

{------------------------------------------------------------------------------}
procedure TFunction.AddParam(value: integer);
var
 fi: TFunctionItem;
begin
  fi := TFunctionItem.Create;
  fi.FInteger := value;
  fi.FType := dtInteger;
  FList.Add(fi);
end;

{------------------------------------------------------------------------------}
procedure TFunction.AddParam(value: TArray);
var
  fi: TFunctionItem;
begin
  fi := TFunctionItem.Create;
  fi.FArray := value;
  fi.FType := dtArray;
  FList.Add(fi);
end;

{------------------------------------------------------------------------------}
procedure TFunction.AddParam(value: TStruct);
var
  fi: TFunctionItem;
begin
  fi := TFunctionItem.Create;
  fi.FStruct := value;
  fi.FType := dtStruct;
  FList.Add(fi);
end;

{------------------------------------------------------------------------------}
procedure TFunction.AddParam(value: boolean);
var
 fi: TFunctionItem;
begin
  fi := TFunctionItem.Create;
  fi.FBoolean := value;
  fi.FType := dtBoolean;
  FList.Add(fi);
end;

{------------------------------------------------------------------------------}
procedure TFunction.AddParamBase64String(value: string);
var
 ti: TFunctionItem;
begin
  ti := TFunctionItem.Create;
  ti.FType := dtBase64;
  ti.FBase64 := MimeEncodeStringNoCRLF(value);
  FList.Add(ti);
end;

{------------------------------------------------------------------------------}
procedure TFunction.AddParamBase64File(filename: string);
var
 tf: TFileStream;
 ti: TFunctionItem;
begin
  ti := TFunctionItem.Create;
  ti.FType := dtBase64;
  tf := TFileStream.Create(filename,fmOpenRead);
  ti.FBase64 :=  MimeEncodeStringNoCRLF(StreamToString(tf));
  FList.Add(ti);
  FreeAndNil(tf);
end;

{------------------------------------------------------------------------------}
procedure TFunction.AddParamDate(value: TDateTime);
var
 fi: TFunctionItem;
begin
  fi := TFunctionItem.Create;
  fi.FDate := value;
  fi.FType := dtDate;
  FList.Add(fi);
end;

{------------------------------------------------------------------------------}
constructor TFunction.Create;
begin
  FList := TObjectList.Create;
end;

{------------------------------------------------------------------------------}
function TFunction.GetRequestXML: string;
var
 index: integer;
 srtn: string;
 ft: TDataType;
begin
  srtn :=            '<?xml version="1.0"?>' + #13#10;
  srtn := srtn + '<methodCall>' + #13#10;
  srtn := srtn + '   <methodName>' +  FMethod  + '</methodName>' +#13#10;
  srtn := srtn + '   <params>' + #13#10;
  for index := 0 to FList.Count -1 do
    begin
      ft := TFunctionItem(FList[index]).FType;
      srtn := srtn + '   <param>' + #13#10;
      case ft of
        dtInteger:
            srtn := srtn + '<value><int>' +
                    IntToStr(TFunctionItem(FList[index]).FInteger) +
                    '</int></value>' + #13#10;
        dtString:
            srtn := srtn + '<value><string>' +
                    TFunctionItem(FList[index]).FString +
                    '</string></value>' + #13#10;

        dtDouble:
            srtn := srtn + '<value><double>' +
                    FloatToStr(TFunctionItem(FList[index]).FDouble) +
                    '</double></value>' + #13#10;

        dtBoolean:
            if TFunctionItem(FList[index]).FBoolean then
              srtn := srtn + '<value><boolean>1</boolean></value>' + #13#10
            else
              srtn := srtn + '<value><boolean>0</boolean></value>' + #13#10;

        dtDate:
            srtn := srtn + '<value><dateTime.iso8601>' +
                    DateTimeToISO(TFunctionItem(FList[index]).FDate) +
                    '</dateTime.iso8601></value>' + #13#10;
        dtArray:
            srtn := srtn + TFunctionItem(FList[index]).FArray.GetAsXML + #13#10;

        dtStruct:
            srtn := srtn + TFunctionItem(FList[index]).FStruct.GetAsXML + #13#10;

        dtBase64:
           srtn := srtn + '<value><base64>' +
                           TFunctionItem(FList[index]).FBase64 +
                           '</base64></value>' + #13#10;
      end;
      srtn := srtn + '   </param>' + #13#10;
    end;
   srtn := srtn + '   </params>' + #13#10;
   result := srtn + '</methodCall>' + #13#10;
end;

{------------------------------------------------------------------------------}
procedure TFunction.Clear;
begin
  FMethod := '';
  FList.Clear;
  FList.Pack;
end;

{------------------------------------------------------------------------------}
destructor TFunction.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TFunction.AddParamBase64Crypto(key, value: string);
var
 fi: TFunctionItem;
 r4: TRC4;
 st: string;
begin
  r4 := TRC4.Create(key);
  st := r4.EncryptString(value);
  fi := TFunctionItem.Create;
  fi.FBase64 := MimeEncodeStringNoCRLF(st);
  fi.FType := dtBase64;
  FList.Add(fi);
  FreeAndNil(r4);
end;

{------------------------------------------------------------------------------}
procedure TResult.AddErrorCode(value: integer);
begin
  FType := dtError;
  FErrorCode := value;
end;

{------------------------------------------------------------------------------}
procedure TResult.AddErrorString(value: string);
begin
  FType := dtError;
  FErrorString := value;
end;

{------------------------------------------------------------------------------}
procedure TResult.AddItem(value: double);
begin
  FType := dtDouble;
  FDouble := value;
end;

{------------------------------------------------------------------------------}
procedure TResult.AddItem(value: integer);
begin
  FType := dtInteger;
  FInteger := value;
end;

{------------------------------------------------------------------------------}
procedure TResult.AddItem(value: string);
begin
  FType := dtString;
  FString := value;
end;

{------------------------------------------------------------------------------}
procedure TResult.AddItem(value: boolean);
begin
  FType := dtBoolean;
  FBoolean := value;
end;

{------------------------------------------------------------------------------}
procedure TResult.AddItem(value: TStruct);
begin
  if value.HasKey('faultCode') then
    begin
      self.SetError;
      FType := dtError;
      self.FErrorCode := value.GetInteger('faultCode');
      self.FErrorString := DecodeEntities(value.GetString('faultString'));
      FreeAndNil(Value); // In case of  a faultcode the struct is no longer used.
                         // mko / 31.08.2003
     exit;
    end;
  FType := dtStruct;
  FStruct := value;
end;

{------------------------------------------------------------------------------}
procedure TResult.AddItem(value: TArray);
begin
  FType := dtArray;
  FArray := value;
end;

{------------------------------------------------------------------------------}
procedure TResult.AddItemDate(value: TDateTime);
begin
  FType := dtDate;
  FDate := value;
end;

{------------------------------------------------------------------------------}
function TResult.GetArray: TArray;
begin
  if (FType = dtArray) then
    result := FArray
  else
    raise Exception.Create('TResult.GetArray - Item is not a array type')
end;

{------------------------------------------------------------------------------}
function TResult.GetBoolean: boolean;
begin

  if (FType = dtBoolean) then
    result := FBoolean
  else
    raise Exception.Create('TResult.GetBoolean - Item is not a boolean type')
end;

{------------------------------------------------------------------------------}
function TResult.GetDate: TDateTime;
begin
  if (FType = dtDate) then
    result := FDate
  else
    raise Exception.Create('TResult.GetDate - Item is not a date type')
end;

{------------------------------------------------------------------------------}
function TResult.GetDouble: double;
begin
  if (FType = dtDouble) then
    result := FDouble
  else
    raise Exception.Create('TResult.GetDouble - Item is not a double type')
end;

{------------------------------------------------------------------------------}
function TResult.GetErrorCode: integer;
begin
  if (FType = dtError) then
    result := FErrorCode
  else
    raise Exception.Create('TResult.GetErrorCode - Item is not a error code type')
end;

{------------------------------------------------------------------------------}
function TResult.GetErrorString: string;
begin
  result := '';
  if (FType = dtError) then
    result := FErrorString
  else
    raise Exception.Create('TResult.GetErrorString - Item is not a error string')
end;

{------------------------------------------------------------------------------}
function TResult.GetInteger: integer;
begin
  if (FType = dtInteger) then
    result := FInteger
  else
    raise Exception.Create('TResult.GetInteger - Item is not a integer type')
end;

{------------------------------------------------------------------------------}
function TResult.GetString: string;
begin
 result := '';
 if (FType = dtString) then
    result := DecodeEntities(FString)
  else
    raise Exception.Create('TResult.GetString - Item is not a string type')
end;

{------------------------------------------------------------------------------}
function TResult.GetStruct: TStruct;
begin
  if (FType = dtStruct) then
    result := FStruct
  else
    raise Exception.Create('TResult.GetStruct - Item is not a struct type')
end;

{------------------------------------------------------------------------------}
function TResult.IsArray: boolean;
begin
  if FType = dtArray then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TResult.IsBase64: boolean;
begin
  if FType = dtBase64 then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TResult.IsBoolean: boolean;
begin
  if FType = dtBoolean then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TResult.IsDate: boolean;
begin
  if FType = dtDate then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TResult.IsDouble: boolean;
begin
  if FType = dtDouble then
    result := true
  else
    result := false;

end;

{------------------------------------------------------------------------------}
function TResult.IsError: boolean;
begin
  if FType = dtError then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TResult.IsInteger: boolean;
begin
  if FType = dtInteger then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TResult.IsString: boolean;
begin
  if FType = dtString then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
function TResult.IsStruct: boolean;
begin
  if FType = dtStruct then
    result := true
  else
    result := false;
end;

{------------------------------------------------------------------------------}
procedure TResult.SetError;
begin
 FType := dtError;
end;

{------------------------------------------------------------------------------}
procedure TResult.AddItemBase64Data(value: string);
begin
  FType := dtBase64;
  FBase64 := value;
end;

{------------------------------------------------------------------------------}
function TResult.GetBase64Stream: TMemoryStream;
begin
  if (FType = dtBase64) then
    begin
      result := TMemoryStream.Create;
      StringToStream(MimeDecodeString(FBase64),result);
    end
  else
    raise Exception.Create('TResult.GetBaseStream - Item is not a base64 type');
end;

{------------------------------------------------------------------------------}
function TResult.GetBase64String: string;
begin
  if (FType = dtBase64) then
    result := MimeDecodeString(FBase64)
  else
    raise Exception.Create('TResult.GetBase64String - Item is not a base64 type')
end;

{------------------------------------------------------------------------------}
procedure TResult.PutBase64ToFile(filename: string);
var
  mstream: TMemoryStream;
begin
  if (FType = dtBase64) then
    begin
      mstream := TMemoryStream.Create;
      StringToStream(MimeDecodeString(FBase64),mstream);
      mstream.SaveToFile(filename);
      FreeAndNil(mstream);
    end
  else
    raise Exception.Create('TResult.PutBase64ToFile - Item is not a base64 type');
end;

{------------------------------------------------------------------------------}
destructor TResult.Destroy;
begin

  if assigned(FArray) then
    FreeAndNil(FArray);
  if assigned(FStruct) then
    FreeAndNil(FStruct);
  FBase64 := '';
  FString := '';

  FType := dtNone;
   inherited;
end;

{------------------------------------------------------------------------------}
procedure TResult.Clear;
begin
  FBase64 :='';
  FBoolean := false;
  FString := '';
  FType := dtNone;
end;

{------------------------------------------------------------------------------}
destructor TFunctionItem.Destroy;
begin
  // Free also FArray und FStruct. 14.8.2003 / mko
  if assigned(FArray) then
    FreeAndNil(FArray);
  if assigned(FStruct) then
    FreeAndNil(FStruct);
  FBase64 := '';
  FString := '';
  inherited;
end;

{------------------------------------------------------------------------------}
destructor TArrayItem.Destroy;
begin
  inherited;
  if assigned(FArray) then
    FreeAndNil(FArray);
  if assigned(FStruct) then
    FreeAndNil(FStruct);
    FBase64 := '';
    FString := '';
end;

{------------------------------------------------------------------------------}
destructor TStructItem.Destroy;
begin
  inherited;
  if assigned(FArray) then
    FreeAndNil(FArray);
  if assigned(FStruct) then
    FreeAndNil(FStruct);
  FBase64 := '';
  FString := '';
end;

{------------------------------------------------------------------------------}
function TFunction.GetResponseXML: string;
var
 index: integer;
 srtn: string;
 ft: TDataType;
begin

  {if we have a error condition return the error instead}
  if FErrorCode > 0 then
    begin
      result := GetErrorXML;
      exit;
    end;


  srtn :=            '<?xml version="1.0"?>' + #13#10;
  srtn := srtn + '<methodResponse>' + #13#10;
  srtn := srtn + '   <params>' + #13#10;
  for index := 0 to FList.Count -1 do
    begin
      ft := TFunctionItem(FList[index]).FType;
      srtn := srtn + '   <param>' + #13#10;
      case ft of
        dtInteger:
            srtn := srtn + '<value><int>' +
                    IntToStr(TFunctionItem(FList[index]).FInteger) +
                    '</int></value>' + #13#10;
        dtString:
            srtn := srtn + '<value><string>' +
                    TFunctionItem(FList[index]).FString +
                    '</string></value>' + #13#10;

        dtDouble:
            srtn := srtn + '<value><double>' +
                    FloatToStr(TFunctionItem(FList[index]).FDouble) +
                    '</double></value>' + #13#10;

        dtBoolean:
            if TFunctionItem(FList[index]).FBoolean then
              srtn := srtn + '<value><boolean>1</boolean></value>' + #13#10
            else
              srtn := srtn + '<value><boolean>0</boolean></value>' + #13#10;

        dtDate:
            srtn := srtn + '<value><dateTime.iso8601>' +
                    DateTimeToISO(TFunctionItem(FList[index]).FDate) +
                    '</dateTime.iso8601></value>' + #13#10;
        dtArray:
            srtn := srtn + TFunctionItem(FList[index]).FArray.GetAsXML + #13#10;

        dtStruct:
            srtn := srtn + TFunctionItem(FList[index]).FStruct.GetAsXML + #13#10;

        dtBase64:
           srtn := srtn + '<value><base64>' +
                           TFunctionItem(FList[index]).FBase64 +
                           '</base64></value>' + #13#10;
      end;
      srtn := srtn + '   </param>' + #13#10;
    end;
   srtn := srtn + '   </params>' + #13#10;
   result := srtn + '</methodResponse>' + #13#10;
end;

{------------------------------------------------------------------------------}
procedure TFunction.AddError(code: integer; msg: string);
begin
  FErrorCode := code;
  FErrorMessage := msg;
end;

{------------------------------------------------------------------------------}
function TFunction.GetErrorXML: string;
var
 msg : string;
begin
  msg := '<?xml version="1.0"?>' + #13#10;
  msg := msg + '<methodResponse>' + #13#10;
  msg := msg + '   <fault>' + #13#10;
  msg := msg + '      <value>' + #13#10;
  msg := msg + '        <struct>' + #13#10;
  msg := msg + '            <member>' + #13#10;
  msg := msg + '               <name>faultCode</name>' + #13#10;
  msg := msg + '               <value><int>' + IntToStr(FErrorCode) + '</int></value>' + #13#10;
  msg := msg + '               </member>' + #13#10;
  msg := msg + '            <member>' + #13#10;
  msg := msg + '               <name>faultString</name>' + #13#10;
  msg := msg + '               <value><string>' + FErrorMessage + '</string></value>' + #13#10;
  msg := msg + '               </member>' + #13#10;
  msg := msg + '            </struct>' + #13#10;
  msg := msg + '         </value>' + #13#10;
  msg := msg + '      </fault>' + #13#10;
  msg := msg + '   </methodResponse>' + #13#10;
  result := msg;
end;

{------------------------------------------------------------------------------}
function TResult.GetBase64Raw: string;
begin
 if (FType = dtBase64) then
    result := FBase64
  else
    raise Exception.Create('TResult.GetBase64String - Item is not a base64 type')
end;

{------------------------------------------------------------------------------}
procedure TArray.SetArray(index: integer; value: TArray);
begin
  try
    TArrayItem(FList[index]).FType := dtArray;
    TArrayItem(FList[index]).FArray := value
  except
    raise exception.Create('TArray.SetArray error during insertion');
  end;
end;

{------------------------------------------------------------------------------}
procedure TArray.SetBase64Data(index: integer; value: TMemoryStream);
begin
  try
    TArrayItem(FList[index]).FType := dtBase64;
    TArrayItem(FList[index]).FBase64 := MimeEncodeStringNoCRLF(StreamToString(value));
  except
    raise exception.Create('TArray.SetBase64Data error during insertion');
  end;
end;

{------------------------------------------------------------------------------}
procedure TArray.SetBase64File(index: integer; FileName: string);
var
 tf: TFileStream;
begin
  try
    tf := TFileStream.Create(FileName,fmOpenRead);
    TArrayItem(FList[index]).FType := dtBase64;
    TArrayItem(FList[index]).FBase64 := MimeEncodeStringNoCRLF(StreamToString(tf));
    FreeAndNil(tf);
  except
    raise exception.Create('TArray.SetBase64File error during insertion');
  end;
end;

{------------------------------------------------------------------------------}
procedure TArray.SetBase64String(index: integer; value: string);
begin
  try
    TArrayItem(FList[index]).FType := dtBase64;
    TArrayItem(FList[index]).FBase64 := MimeEncodeStringNoCRLF(value);
  except
    raise exception.Create('TArray.SetBase64String error during insertion');
  end;
end;

{------------------------------------------------------------------------------}
procedure TArray.SetBoolean(index: integer; value: boolean);
begin
  try
    TArrayItem(FList[index]).FType := dtBoolean;
    TArrayItem(FList[index]).FBoolean := value;
  except
    raise exception.Create('TArray.SetBoolean error during insertion');
  end;
end;

{------------------------------------------------------------------------------}
procedure TArray.SetDate(index: integer; value: TDateTime);
begin
  try
    TArrayItem(FList[index]).FType := dtDate;
    TArrayItem(FList[index]).FDate := value;
  except
    raise exception.Create('TArray.SetDouble error during insertion');
  end;
end;

{------------------------------------------------------------------------------}
procedure TArray.SetDouble(index: integer; value: double);
begin
  try
    TArrayItem(FList[index]).FType := dtDouble;
    TArrayItem(FList[index]).FDouble := value;
  except
    raise exception.Create('TArray.SetDouble error during insertion');
  end;
end;

{------------------------------------------------------------------------------}
procedure TArray.SetInteger(index, value: integer);
begin
  try
    TArrayItem(FList[index]).FType := dtInteger;
    TArrayItem(FList[index]).FInteger := value;
  except
    raise exception.Create('TArray.SetInteger error during insertion');
  end;
end;

{------------------------------------------------------------------------------}
procedure TArray.SetString(index: integer; value: string);
begin
  try
    TArrayItem(FList[index]).FType := dtString;
    TArrayItem(FList[index]).FString := value;
  except
    raise exception.Create('TArray.SetString error during insertion');
  end;
end;

{------------------------------------------------------------------------------}
procedure TArray.SetStruct(index: integer; value: TStruct);
begin
  try
    TArrayItem(FList[index]).FType := dtStruct;
    TArrayItem(FList[index]).FStruct := value;
  except
    raise exception.Create('TArray.SetStruct error during insertion');
  end;
end;

{------------------------------------------------------------------------------}
procedure TStruct.Clear;
var
  i: integer;
begin
  for i := 0 to FItemList.Count -1 do
    TStructItem(FItemList.Objects[i]).Free;
  FItemList.Clear;
end;

{------------------------------------------------------------------------------}
procedure TStruct.Delete(key: string);
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.Delete(' + key + ') - key not found');
  TStructItem(FItemList.Objects[i]).Free;
  FItemList.Delete(i);
end;

{------------------------------------------------------------------------------}
procedure TStruct.SetArray(key: string; value: TArray);
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.SetArray(' + key + ') - key not found');

  TStructItem(FItemList.Objects[i]).FType := dtArray;
  TStructItem(FItemList.Objects[i]).FArray := value;
end;

{------------------------------------------------------------------------------}
procedure TStruct.SetBase64Data(key: string; value: TMemoryStream);
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.SetBase64String(' + key + ') - key not found');

  TStructItem(FItemList.Objects[i]).FType := dtBase64;
  TStructItem(FItemList.Objects[i]).FBase64 := MimeEncodeStringNoCRLF(StreamToString(value));
end;

{------------------------------------------------------------------------------}
procedure TStruct.SetBase64File(key, FileName: string);
var
 i: integer;
 tf: TFileStream;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.SetBase64String(' + key + ') - key not found');

  try
    tf := TFileStream.Create(FileName,fmOpenRead);
    TStructItem(FItemList.Objects[i]).FType := dtBase64;
    TStructItem(FItemList.Objects[i]).FBase64 := MimeEncodeStringNoCRLF(StreamToString(tf));
    FreeAndNil(tf);
  except
    raise Exception.Create('TStruct.SetBase64File insertion failed')
  end;
end;

{------------------------------------------------------------------------------}
procedure TStruct.SetBase64String(key, value: string);
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.SetBase64String(' + key + ') - key not found');

  TStructItem(FItemList.Objects[i]).FType := dtBase64;
  TStructItem(FItemList.Objects[i]).FBase64 := MimeEncodeStringNoCRLF(value);
end;

{------------------------------------------------------------------------------}
procedure TStruct.SetBoolean(key: string; value: boolean);
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.SetBoolean(' + key + ') - key not found');

  TStructItem(FItemList.Objects[i]).FType := dtBoolean;
  TStructItem(FItemList.Objects[i]).FBoolean := value;
end;

{------------------------------------------------------------------------------}
procedure TStruct.SetDate(key: string; value: TDateTime);
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.SetDate(' + key + ') - key not found');

  TStructItem(FItemList.Objects[i]).FType := dtDate;
  TStructItem(FItemList.Objects[i]).FDate := value;
end;

{------------------------------------------------------------------------------}
procedure TStruct.SetDouble(key: string; value: double);
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.SetDouble(' + key + ') - key not found');

  TStructItem(FItemList.Objects[i]).FType := dtDouble;
  TStructItem(FItemList.Objects[i]).FDouble := value;
end;

{------------------------------------------------------------------------------}
procedure TStruct.SetInteger(key: string; value: integer);
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.SetInteger(' + key + ') - key not found');

  TStructItem(FItemList.Objects[i]).FType := dtInteger;
  TStructItem(FItemList.Objects[i]).FInteger := value;
end;

{------------------------------------------------------------------------------}
procedure TStruct.SetString(key, value: string);
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.SetString(' + key + ') - key not found');

  TStructItem(FItemList.Objects[i]).FType := dtString;
  TStructItem(FItemList.Objects[i]).FString := value;
end;

{------------------------------------------------------------------------------}
procedure TStruct.SetStruct(key: string; value: TStruct);
var
 i: integer;
begin
  i := FItemList.IndexOf(key);
  if i < 0 then
    raise Exception.Create('TStruct.SetStruct(' + key + ') - key not found');

  TStructItem(FItemList.Objects[i]).FType := dtStruct;
  TStructItem(FItemList.Objects[i]).FStruct := value;
end;

end.
