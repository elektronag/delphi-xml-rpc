
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/source/xmlrpcserver.pas,v 1.1.1.1 2003-11-19 22:11:48 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit xmlrpcserver;

interface

uses
  Classes,
  IdHTTPServer,
  IdTCPServer,
  xmlrpccommon,
  xmlrpcparser,
  xmlrpctypes,
  contnrs,
  sysutils,
  syncobjs;

type
  { method handler procedure type }
  TRPCMethod = procedure(const MethodName: string; const plist: TList; const return: TReturn) of object;

  { method pointer object }
  TMethodHandler = class(TObject)
   Name: string;
   Method: TRPCMethod;
   Help: string;
   Signature: string;
  end;

  { server params parser }
  TServerParser = class(TObject)
  private
    FParser : TXMLParser;
    FStack: TStack;
    FLastTag: string;
    FName: string;
    FMethodName: string;
  public
   constructor Create;
   destructor Destroy; override;
   property  RequestName: string read FMethodName write FMethodName;
   procedure Parse(sdata: string);
   function GetParameters: TObjectList;
   procedure StartTag;
   procedure EndTag;
   procedure DataTag;
  end;

  { the actual server object }
  TServer = class(TObject)
  private
    FServer : TIdHttpServer;
    FPort: integer;
    FMethodList: TList;
    FSParser: TServerParser;
    FActive: boolean;
    FLock : TCriticalSection;
    FIntrospect: boolean;

    {introspection extension methods }
    procedure system_listMethods(const MethodName: string; const plist: TList; const return: TReturn);
    procedure system_methodHelp(const MethodName: string; const plist: TList; const return: TReturn);
    procedure system_methodSignature(const MethodName: string; const plist: TList; const return: TReturn);
    { end introspection extension methods }

    procedure DataPosted(AThread: TIdPeerThread;
                         RequestInfo: TIdHTTPRequestInfo;
                         ResponseInfo: TIdHTTPResponseInfo);
    procedure SetActive(const Value: boolean);
    procedure StartServer;
    procedure StopServer;
    function GetParser : TServerParser;
  public
    constructor Create;
    destructor Destroy; override;
    property EnableIntrospect: boolean read FIntrospect write FIntrospect;
    procedure RegisterMethodHandler(hndlr: TMethodHandler);
    property ListenPort: integer read FPort write FPort;
    property Active : boolean read FActive write SetActive;
  end;

implementation

{------------------------------------------------------------------------------}
constructor TServerParser.Create;
begin
  FParser := TXMLParser.Create;
  FStack := TStack.Create;
end;

{------------------------------------------------------------------------------}
destructor TServerParser.Destroy;
begin
  if Assigned(fStack) then
    fStack.Free;
  if Assigned(fParser) then
    FParser.Free;
  inherited;
end;

{------------------------------------------------------------------------------}
procedure TServerParser.DataTag;
var
  dat : string;
begin
  dat := FParser.CurContent;

  { nothing empty allowed }
  if not(trim(dat) <> '') then
    exit;

  if(FLastTag = 'METHODNAME') then
    FMethodName := dat;

  {this will handle the default
   string pain in the ass}
  if FLastTag = 'VALUE' then
    FLastTag := 'STRING';

  {ugly null string hack}
    if(FLastTag = 'STRING') then
      if(dat = '[NULL]') then
        dat := '';

  {if the tag was a struct name we will
   just store it for the next pass    }
  if FLastTag = 'NAME' then
    begin
      FName := dat;
      exit;
    end;

  if (FStack.Count > 0) then
    if (TObject(FStack.Peek) is TStruct) then
      begin
        if (FLastTag = 'STRING') then
          TStruct(FStack.Peek).LoadRawData(dtString,FName,dat);
        if (FLastTag = 'INT') then
          TStruct(FStack.Peek).LoadRawData(dtInteger,FName,dat);
        if (FLastTag = 'INT4') then
          TStruct(FStack.Peek).LoadRawData(dtDate,FName,dat);
        if (FLastTag = 'DOUBLE') then
          TStruct(FStack.Peek).LoadRawData(dtDouble,FName,dat);
        if (FLastTag = 'DATETIME.ISO8601') then
          TStruct(FStack.Peek).LoadRawData(dtDate,FName,dat);
        if (FLastTag = 'BASE64') then
          TStruct(FStack.Peek).LoadRawData(dtBase64,FName,dat);
        if (FLastTag = 'BOOLEAN') then
          TStruct(FStack.Peek).LoadRawData(dtBoolean,FName,dat);

      end;

  if (FStack.Count > 0) then
    if (TObject(FStack.Peek) is TArray) then
      begin
        if (FLastTag = 'STRING') then
          TArray(FStack.Peek).LoadRawData(dtString,dat);
        if (FLastTag = 'INT') then
          TArray(FStack.Peek).LoadRawData(dtInteger,dat);
        if (FLastTag = 'INT4') then
          TArray(FStack.Peek).LoadRawData(dtInteger,dat);
        if (FLastTag = 'DOUBLE') then
        TArray(FStack.Peek).LoadRawData(dtDouble,dat);
        if (FLastTag = 'DATETIME.ISO8601') then
          TArray(FStack.Peek).LoadRawData(dtDate,dat);
        if (FLastTag = 'BASE64') then
          TArray(FStack.Peek).LoadRawData(dtBase64,dat);
        if (FLastTag = 'BOOLEAN') then
          TArray(FStack.Peek).LoadRawData(dtBoolean,dat);
       end;

  {here we are just getting a single value}
  if FStack.Count > 0 then
   if (TObject(FStack.Peek) is TParameter) then
    begin
      if (FLastTag = 'STRING') then
        TParameter(FStack.Peek).AddItem(dat);
      if (FLastTag = 'INT') then
        TParameter(FStack.Peek).AddItem(StrToInt(dat));
      if (FLastTag = 'INT4') then
        TParameter(FStack.Peek).AddItem(StrToInt(dat));
      if (FLastTag = 'DOUBLE') then
        TParameter(FStack.Peek).AddItem(StrToFloat(dat));
      if (FLastTag = 'DATETIME.ISO8601') then
        TParameter(FStack.Peek).AddItemDate(ISOToDateTime(dat));
      if (FLastTag = 'BASE64') then
        TParameter(FStack.Peek).AddItemBase64Data(dat);
      if (FLastTag = 'BOOLEAN') then
        if (dat = '0') then
          TParameter(FStack.Peek).AddItem(false)
        else
          TParameter(FStack.Peek).AddItem(true);
    end;
end;

{------------------------------------------------------------------------------}

procedure TServerParser.EndTag;
var
  sTag: string;
  sa: TArray;
  st: TStruct;
begin
  FLastTag := '';

  sTag := UpperCase(Trim(string(FParser.CurName)));

  if(sTag = 'ARRAY') then
    if(TObject(FStack.Peek) is TArray) then
      begin
        sa := TArray(FStack.Pop);
        if (TObject(FStack.Peek) is TParameter) then
          TParameter(FStack.Peek).addItem(sa);
        if (TObject(FStack.Peek) is TArray) then
          TArray(FStack.Peek).addItem(sa);
        if (TObject(FStack.Peek) is TStruct) then
          TStruct(FStack.Peek).addItem(FName, sa);
      end;

    if(sTag = 'STRUCT') then
    if(TObject(FStack.Peek) is TStruct) then
      begin
        st := TStruct(FStack.Pop);
        if (TObject(FStack.Peek) is TParameter) then
          TParameter(FStack.Peek).addItem(st);
        if (TObject(FStack.Peek) is TArray) then
          TArray(FStack.Peek).addItem(st);
        if (TObject(FStack.Peek) is TStruct) then
          TStruct(FStack.Peek).addItem(FName, st);
      end;
end;

{------------------------------------------------------------------------------}

function TServerParser.GetParameters: TObjectList;
var
 index: integer;
begin
 Result := TObjectList.Create;
 {we need to reverse the order of the items in the list
  to make the order appear correct at the called method}
 Result.Count := FStack.Count;
 for index := FStack.Count -1 downto 0 do
   Result[index] := TParameter(FStack.Pop);

end;


{------------------------------------------------------------------------------}
procedure TServerParser.Parse(sdata: string);
begin
 FParser.LoadFromBuffer(pchar(sdata));
 FParser.StartScan;
 FParser.Normalize := False;
  while FParser.Scan do
    begin
      case FParser.CurPartType of
           ptStartTag:
             StartTag;
           ptContent:
             DataTag;
           ptEndTag:
             EndTag;
      end;
    end;
end;

{------------------------------------------------------------------------------}
procedure TServerParser.StartTag;
var
  sTag: string;
  tp: TParameter;
  st: TStruct;
  sa: Tarray;

begin
  sTag := UpperCase(Trim(string(FParser.CurName)));

  if(sTag = 'PARAM') then
    begin
      tp := TParameter.Create;
      FStack.Push(tp);
    end;
  if(sTag = 'ARRAY') then
    begin
      sa := TArray.Create;
      FStack.Push(sa);
    end;
  if(sTag = 'STRUCT') then
    begin
      st := TStruct.Create;
      FStack.Push(st);
    end;

  FLastTag := sTag;
end;

{------------------------------------------------------------------------------}
constructor TServer.Create;
begin
  FServer := TIdHTTPServer.Create(nil);
  FSParser := TServerParser.Create;
  FMethodList := TList.Create;
  FLock := TCriticalSection.Create;

end;

{------------------------------------------------------------------------------}




procedure TServer.DataPosted(AThread: TIdPeerThread;
  RequestInfo: TIdHTTPRequestInfo; ResponseInfo: TIdHTTPResponseInfo);
var
  index: integer;
  return: TReturn;
  FFound: boolean;
  pList : TList;
  RequestName : string;
  AParser : TServerParser;
begin
  FFound := false;

  return := TReturn.Create;
  try

    { PMM alteration }
    Flock.Acquire;
    try

    { this section is not thread-safe, as all values going to and from
      the parser are not passed in and out on the stack }
    { everything else is on the stack, or presumed safe to use }
    { If the parser where some kind of factory object, that merely supplied processing
      functionality, the former approach would be fine (and very straightforward) }
     { however, the results are in the internal state of the parser, thus in principle,
       if threadA and threadB call into this, the parser will become very upset }
     { there are several possible approaches for fixes : }
     { [a] lock the parser object for the duration that it's state needs to be protected
          this means not only for while the parse results are being used,
          but when it's being cleared etc. or anything else
          - this could get messy real quick, and one has to be damn confident everything is threadsafe
          - V hard to do}
     { [b] forget making the parser threadsafe, and make the access to the parser(s) used threadsafe
          this means either serialise all requests - poor performance
          or never give the same parser to different threads }
      { note that I've done [a] as it *seems* to be OK }
      { this approach is fine,
        as long as parsing in not the major job or processing the request,
        and normally, it shouldn't be }
      { if parsing is intensive, then GetParser needs to be implemented
        to allow per-thread parser allocation }


      AParser := GetParser;
      AParser.Parse(RequestInfo.UnparsedParams);
      { PMM alteration }
      pList := AParser.GetParameters;
      RequestName := AParser.RequestName;
       { note that the parser lock can be dropped after here, in principle }

    finally
      FLock.Release;
    end;

    for index := 0 to FMethodList.Count -1 do
     if (TMethodHandler(FMethodList[index]).Name = RequestName) then
       begin

         TMethodHandler(FMethodList[index]).Method(TMethodHandler(FMethodList[index]).Name,pList, return);
         pList.Free;
         FFound := true;
       end;

    if not FFound then
      begin
        return.AddError(999,'Requested method was not registered on the server');
        ResponseInfo.ContentType := 'text/xml';
        ResponseInfo.ServerSoftware := 'CODEPUNK XMLRPC SERVER';
        ResponseInfo.ContentText := return.GetResponseXML;
        return.Free;
        exit;
      end;

    ResponseInfo.ContentType := 'text/xml';
    ResponseInfo.ServerSoftware := 'CODEPUNK XMLRPC SERVER';
    ResponseInfo.ContentText := return.GetResponseXML;
    return.Free;
  except
    on E: Exception do
      begin
        return.AddError(999,E.Message);
        ResponseInfo.ContentType := 'text/xml';
        ResponseInfo.ServerSoftware := 'CODEPUNK XMLRPC SERVER';
        ResponseInfo.ContentText := return.GetResponseXML;
        return.Free;
      end;

  end;

end;

{------------------------------------------------------------------------------}
destructor TServer.Destroy;
begin
  FLock.Free;
  FMethodList.Free;
  FSParser.Free;
  FServer.Free;
end;

{------------------------------------------------------------------------------}
function TServer.GetParser: TServerParser;
begin
  { dummy allocator }
  Result := FSParser;
end;

{------------------------------------------------------------------------------}
procedure TServer.RegisterMethodHandler(hndlr: TMethodHandler);
begin
  FMethodList.Add(hndlr);
end;

{------------------------------------------------------------------------------}
procedure TServer.SetActive(const Value: boolean);
begin
  if (Factive <> Value) then
  begin
    if Value then
      StartServer
    else
      StopServer;
    FActive := Value;
  end;
end;

{------------------------------------------------------------------------------}
procedure TServer.StartServer;
var
  meth: TMethodHandler;
begin
  FServer.DefaultPort := FPort;
  FServer.OnCommandGet := DataPosted;
  FServer.SessionTimeOut := 10000;
  {introspection extension}
  if FIntrospect then
    begin
      {list methods}
      meth := TMethodHandler.Create;
      meth.Name := 'system.listMethods';
      meth.Method := system_listMethods;
      meth.Signature := 'array (no params)';
      meth.Help := 'Returns a list  of all methods registered with the server';
      registerMethodHandler(meth);
      {system help}
      meth := TMethodHandler.Create;
      meth.Name := 'system.methodHelp';
      meth.Method := system_methodHelp;
      meth.Signature := 'string (string method_name)';
      meth.Help := 'Returns any help text supplied for this method';
      registerMethodHandler(meth);
      {system signature}
      meth := TMethodHandler.Create;
      meth.Name := 'system.methodSignature';
      meth.Method := system_methodSignature;
      meth.Signature := 'array of arrays (string method_name)';
      meth.Help := 'Returns a array of know signatures for this method';
      registerMethodHandler(meth);
    end;









  FServer.Active := True;
end;

{------------------------------------------------------------------------------}
procedure TServer.StopServer;
begin
  FServer.Active := false;
end;

{------------------------------------------------------------------------------}
{server introspection extensions}
procedure TServer.system_listMethods(const MethodName: string; const plist: TList; const return: TReturn);
var
 ar : TArray;
 index: integer;
begin
  ar := TArray.Create;
  for index := 0 to FMethodList.Count -1 do
    ar.AddItem(TMethodHandler(FMethodList[index]).Name);

  return.AddParam(ar);
end;

{------------------------------------------------------------------------------}
procedure TServer.system_methodHelp(const MethodName: string; const plist: TList; const return: TReturn);
var
 szSent: string;
 index: integer;
begin
 szSent := trim(TParameter(plist[0]).GetString);
 for index := 0 to FMethodList.Count -1 do
   if (TMethodHandler(FMethodList[index]).Name = szSent) then
      szSent := TMethodHandler(FMethodList[index]).Help;
 return.AddParam(szSent);
end;

{------------------------------------------------------------------------------}
procedure TServer.system_methodSignature(const MethodName: string; const plist: TList; const return: TReturn);
var
szSent: string;
aa: TArray;
ab: TArray;
index: integer;
begin
  szSent := trim(TParameter(plist[0]).GetString);
  aa := TArray.Create;
  ab := TArray.Create;
  for index := 0 to FMethodList.Count -1 do
    if (TMethodHandler(FMethodList[index]).Name = szSent) then
       ab.addItem(TMethodHandler(FMethodList[index]).Signature);
  aa.addItem(ab);
  return.AddParam(aa);
end;

end.
