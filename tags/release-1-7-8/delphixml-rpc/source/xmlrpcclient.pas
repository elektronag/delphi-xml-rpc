
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/source/xmlrpcclient.pas,v 1.1.1.1 2003-11-19 22:12:11 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit xmlrpcclient;

interface
uses
  classes,
  sysutils,
  xmlrpctypes,
  xmlrpccommon,
  IdHTTP,
  {$IFDEF INDY9}
  IdHashMessageDigest,
  IdHash,
  {$ENDIF}
  IdSSLOpenSSL,
  xmlrpcparser,
  contnrs;

type

  TClientParser = class(TObject)
  private
    FStack: TObjectStack;
    FName: string;
    FResult: TResult;
    FParser: TXMLParser;
    FLastTag: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(sData: string);
    procedure StartTag;
    procedure EndTag;
    procedure DataTag;
    function GetResult: TResult;
  end;

  TCaller = class(TClientParser)
    private
      FHostName: string;
      FHostPort: integer;
      FProxyName: string;
      FProxyPort: integer;
      FProxyUserName: string;
      FProxyPassword: string;
      FSSLEnable: boolean;
      FSSLRootCertFile: string;
      FSSLCertFile: string;
      FSSLKeyFile: string;
      FEndPoint: string;
      FProxyBasicAuth: boolean;
      FFixEmptyString: boolean;
      function Post(rdata: string): string;
    public
      constructor Create;
      property FixEmptyStrings: boolean read FFixEmptyString write FFixEmptyString;
      property EndPoint: string read FEndPoint write FEndPoint;
      property HostName: string read FHostName write FHostName;
      property HostPort: integer read FHostPort write FHostPort;
      property ProxyName: string read FProxyName write FProxyName;
      property ProxyPort: integer read FProxyPort write FProxyPort;
      property ProxyUserName: string read FProxyUserName write FProxyUserName;
      property ProxyPassword: string read FProxyPassword write FProxyPassword;
      property ProxyBasicAuth: boolean read FProxyBasicAuth write FProxyBasicAuth;
      property SSLEnable: boolean read FSSLEnable write FSSLEnable;
      property SSLRootCertFile: string read FSSLRootCertFile write FSSLRootCertFile;
      property SSLCertFile: string read FSSLCertFile write FSSLCertFile;
      property SSLKeyFile: string read FSSLKeyFile write FSSLKeyFile;
      {$IFDEF INDY9}
      function Execute(value: TFunction; ttl: integer): TResult; overload;
      {$ENDIF}

      function Execute(value: string): TResult;overload;


      function Execute(value: TFunction): TResult; overload;
      procedure DeleteOldCache(ttl: integer);
    end;

const
  ERROR_EMPTY_RESULT = 600;
  ERROR_EMPTY_RESULT_MESSAGE = 'The xml-rpc server returned a empty response';
  ERROR_INVALID_RESPONSE = 601;
  ERROR_INVALID_RESPONSE_MESSAGE = 'Invalid payload received from xml-rpc server';

implementation

{------------------------------------------------------------------------------}
{ RPC PARSER CONSTRUCTOR                                                       }
{------------------------------------------------------------------------------}
constructor TClientParser.Create;
begin
  inherited;
end;


destructor TClientParser.Destroy;
begin

  if Assigned(FStack) then
  begin
    FStack.Free;
  end;
  if Assigned(FParser) then
    FParser.Free;

  inherited;

end;


{------------------------------------------------------------------------------}
{ RETURN THE RESULT OBJECT  tastes great less filling ;)                      }
{------------------------------------------------------------------------------}
function TClientParser.GetResult: TResult;
begin
  result := FResult;
end;

{------------------------------------------------------------------------------}
procedure TClientParser.Parse(sData: string);
begin

  if not Assigned(FParser) then
    FParser := TXMLParser.Create;
  if not Assigned(FStack) then
    FStack := TObjectStack.Create;
  if not Assigned(FResult) then
    FResult := TResult.Create;

  FParser.LoadFromBuffer(pchar(sData));
  FParser.StartScan;
  FParser.Normalize := false;
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
{ CACHED WEB CALL Time To Live calculated in minutes                           }
{------------------------------------------------------------------------------}
{$IFDEF INDY9}
function TCaller.Execute(value: TFunction; ttl: integer): TResult;
var
  sl: TStrings;
  rs: string;
  rq: string;
  fn: string;
  md : TIdHashMessageDigest5;
begin
  rq := value.GetRequestXML;

  md := TIdHashMessageDigest5.Create;
  sl := TStringList.Create;

  { determine the md5 digest hash of the request }
  fn := Hash128AsHex(md.HashValue(rq));

  { if we have a cached file from a previous request
    that has not expired then load it }
  if FileExists(GetTempDir + fn + '.csh') then
    if not FileIsExpired(GetTempDir + fn + '.csh',ttl) then
      begin
        sl.LoadFromFile(GetTempDir + fn + '.csh');

        if(FFixEmptyString) then
          Parse(FixEmptyString(sl.Text))
        else
          Parse(sl.Text);

        sl.Free;
        value.Clear;
        result := GetResult;
        exit;
      end;

  { ok we got here so we where expired or did not exist
    make the call and cache the result this time }
  rs := Post(rq);



  {simple error check}
   if not (pos('xml',rs)  > 0) then
     begin
       result := TResult.Create;
       result.SetError;
       result.AddErrorCode(ERROR_INVALID_RESPONSE);
       result.AddErrorString(ERROR_INVALID_RESPONSE_MESSAGE);
       EXIT;
     end;

  {empty response}
   if (trim(rs) = '') then
     begin
       result := TResult.Create;
       result.SetError;
       result.AddErrorCode(ERROR_EMPTY_RESULT);
       result.AddErrorString(ERROR_EMPTY_RESULT_MESSAGE);
       EXIT;
     end;



   Parse(rs);

   sl.Text := rs;
   sl.SaveToFile(GetTempDir + fn + '.csh');
   sl.Free;
   value.Clear;
   result := GetResult;

end;
{$ENDIF}

{------------------------------------------------------------------------------}
{ DELETE ALL TEMPORARY EXPIRED DATA                                            }
{------------------------------------------------------------------------------}
procedure TCaller.DeleteOldCache(ttl: integer);
var r : integer;
    SR : tSearchRec;
begin
  r := FindFirst(GetTempDir + '*.csh', faAnyFile, SR);
  if r = 0 then begin
    while r = 0 do begin
      if (SR.attr and faDirectory = 0) then begin
        if FileIsExpired(GetTempDir + SR.Name,ttl) then
          begin
           DeleteFile(GetTempDir + SR.Name);
        end;
      end;
      r := FindNext(SR);
    end;
    FindClose(SR);
  end;
end;

{------------------------------------------------------------------------------}
{ NON - CACHED WEB CALL                                                        }
{------------------------------------------------------------------------------}
function TCaller.Execute(value: TFunction): TResult;
var
 rs: string;

begin
  rs := Post(value.GetRequestXML);
  { empty string fix  }
  if(FFixEmptyString) then
    rs := FixEmptyString(rs);

   {simple error check}
   if not (pos('xml',rs)  > 0) then
     begin
       result := TResult.Create;
       result.SetError;
       result.AddErrorCode(ERROR_INVALID_RESPONSE);
       result.AddErrorString(ERROR_INVALID_RESPONSE_MESSAGE);
       EXIT;
     end;

  {empty response}
   if (trim(rs) = '') then
     begin
       result := TResult.Create;
       result.SetError;
       result.AddErrorCode(ERROR_EMPTY_RESULT);
       result.AddErrorString(ERROR_EMPTY_RESULT_MESSAGE);
       EXIT;
     end;

     Parse(rs);
     value.Clear;
     result := GetResult;


end;

{------------------------------------------------------------------------------}
{ POST THE REQUEST TO THE RPC SERVER                                           }
{------------------------------------------------------------------------------}
function TCaller.Post(rdata: string): string;
var
 rsp: TMemoryStream;
 snd: TMemoryStream;
 FSession : TIdHttp;
 SSLIOHandler: TIdSSLIOHandlerSocket;
begin
  rsp := TMemoryStream.Create;
  snd := TMemoryStream.Create;
  StringToStream(rdata,snd); { convert to a stream }
  FSession := TIdHTTP.Create(nil);

  SSLIOHandler := nil;
  if (FSSLEnable) then
    begin
      SSLIOHandler := TIdSSLIOHandlerSocket.Create(nil);
      SSLIOHandler.SSLOptions.RootCertFile := FSSLRootCertFile;
      SSLIOHandler.SSLOptions.CertFile := FSSLCertFile;
      SSLIOHandler.SSLOptions.KeyFile :=  FSSLKeyFile;
      FSession.IOHandler := SSLIOHandler;
    end;

  { proxy setup }
  if (FProxyName <> '') then
    begin
      {proxy basic auth}
      if(FProxyBasicAuth) then
        FSession.ProxyParams.BasicAuthentication := true;

      FSession.ProxyParams.ProxyServer := FProxyName;
      FSession.ProxyParams.ProxyPort := FProxyPort;
      FSession.ProxyParams.ProxyUsername := FProxyUserName;
      FSession.ProxyParams.ProxyPassword := FProxyPassword;
    end;

  FSession.Request.Accept := '*/*';
  FSession.Request.ContentType := 'text/xml';
  FSession.Request.Connection := 'Keep-Alive';
  FSession.Request.ContentLength := length(rdata);
  if not FSSLEnable then
    if FHostPort = 80 then
      FSession.Post('http://' + FHostName +  FEndPoint,snd,rsp)
    else
      FSession.Post('http://' + FHostName + ':' + IntToStr(FHostPort) + FEndPoint,snd,rsp);

  if FSSLEnable then
    FSession.Post('https://' + FHostName + ':' + IntToStr(FHostPort) + FEndPoint,snd,rsp);

  result := StreamToString(rsp);
  FSession.Free;

  if Assigned(SSLIOHandler) then
    SSLIOHandler.Free;

  rsp.Free;
  snd.Free;
end;


{------------------------------------------------------------------------------}
constructor TCaller.Create;
begin
  inherited;
  FHostPort := 80;
  FSSLEnable := false;
  FProxyBasicAuth := false;
end;



{------------------------------------------------------------------------------}
procedure TClientParser.DataTag;
var
  dat : string;
begin
  dat := FParser.CurContent;
  { should never be empty }
  if not(trim(dat) <> '') then
    exit;
  { last tag empty ignore }
  if(FLastTag = '') then
    exit;

  { struct name store for next pass}
  if(FLastTag = 'NAME') then
    if not (trim(dat) <> '') then
      exit;

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
  if (FLastTag = 'NAME')then
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
        if (FLastTag = 'I4') then
          TStruct(FStack.Peek).LoadRawData(dtInteger,FName,dat);
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
        if (FLastTag = 'I4') then
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
  if FStack.Count = 0 then
    begin
      if (FLastTag = 'STRING') then
        FResult.AddItem(dat);
      if (FLastTag = 'INT') then
        FResult.AddItem(StrToInt(dat));
      if (FLastTag = 'I4') then
        FResult.AddItem(StrToInt(dat));
      if (FLastTag = 'DOUBLE') then
        FResult.AddItem(StrToFloat(dat));
      if (FLastTag = 'DATETIME.ISO8601') then
         FResult.AddItemDate(ISOToDateTime(dat));
      if (FLastTag = 'BASE64') then
         FResult.AddItemBase64Data(dat);
      if (FLastTag = 'BOOLEAN') then
        if (dat = '0') then
          FResult.AddItem(false)
        else
          FResult.AddItem(true);
    end;


      FLastTag := '';
end;

{------------------------------------------------------------------------------}
procedure TClientParser.EndTag;
var
 st : TStruct;
 sa : TArray;
 sTag: string;
begin
  sTag := UpperCase(Trim(string(FParser.CurName)));

  {if we get a struct closure then
   we pop it off the stack do a peek on
   the item before it and add  it}
  if(sTag = 'STRUCT') then
  begin
    {last item is a struct}
    if(TObject(FStack.Peek) is TStruct) then
      if(FStack.Count > 0) then
        begin
          st := TStruct(FStack.Pop);
          if(FStack.Count > 0) then
            begin
              if (TObject(FStack.Peek) is TArray) then
                TArray(FStack.Peek).AddItem(st);
              if (TObject(FStack.Peek) is TStruct) then
                TStruct(FStack.Peek).AddItem(FName,st)
            end
           else
             FResult.AddItem(st);
         exit;
        end;

     {last item is a array}
     if(TObject(FStack.Peek) is TArray) then
       if(FStack.Count > 0) then
         begin
           sa := TArray(FStack.Pop);
           if(FStack.Count > 0) then
             begin
               if (TObject(FStack.Peek) is TArray) then
                 TArray(FStack.Peek).AddItem(sa);
               if (TObject(FStack.Peek) is TStruct) then
                 TStruct(FStack.Peek).AddItem(FName,sa);
             end
           else
             FResult.AddItem(sa);
          exit;
         end;
  end;

  if(sTag = 'ARRAY') then
  begin
    if(TObject(FStack.Peek) is TArray) then
      if(FStack.Count > 0) then
        begin
          sa := TArray(FStack.Pop);
          if(FStack.Count > 0) then
            begin
              if(TObject(FStack.Peek) is TStruct) then
                TStruct(FStack.Peek).AddItem(FName,sa);
              if(TObject(FStack.Peek) is TArray) then
                TArray(FStack.Peek).AddItem(sa);
            end
          else
            FResult.AddItem(sa);
            exit;
       end;
  end;

  {if we get the params closure then we will pull the array
   and or struct and add it to the final result then clean up}
 if(sTag = 'PARAMS') then
   if(FStack.Count > 0) then
   begin
     if(TObject(FStack.Peek) is TStruct) then
       FResult.AddItem(TStruct(FStack.Pop));
     if(TObject(FStack.Peek) is TArray) then
       FResult.AddItem(TArray(FStack.Pop));
     {free the stack}
     FStack.Free;
   end;
end;

{------------------------------------------------------------------------------}
procedure TClientParser.StartTag;
var
 sTag: string;
 st: TStruct;
 sa : TArray;
begin
  sTag := UpperCase(Trim(string(FParser.CurName)));

  if(sTag = 'STRUCT') then
    begin
      st := TStruct.Create;
      FStack.Push(st);
    end;

  if(sTag = 'ARRAY') then
    begin
      sa := TArray.Create;
      FStack.Push(sa);
    end;

  FLastTag := sTag;
end;


function TCaller.Execute(value: string): TResult;
var
 rs: string;

begin
  rs := Post(value);
  { empty string fix }
  if(FFixEmptyString) then
    rs := FixEmptyString(rs);

  {simple error check}
   if not (pos('xml',rs)  > 0) then
     begin
       result := TResult.Create;
       result.SetError;
       result.AddErrorCode(ERROR_INVALID_RESPONSE);
       result.AddErrorString(ERROR_INVALID_RESPONSE_MESSAGE);
       EXIT;
     end;

  {empty response}
   if (trim(rs) = '') then
     begin
       result := TResult.Create;
       result.SetError;
       result.AddErrorCode(ERROR_EMPTY_RESULT);
       result.AddErrorString(ERROR_EMPTY_RESULT_MESSAGE);
       EXIT;
     end;


     Parse(rs);
     result := GetResult;

end;


end.
