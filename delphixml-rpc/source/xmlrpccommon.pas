
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/source/xmlrpccommon.pas,v 1.1.1.1 2003-11-19 22:11:51 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit xmlrpccommon;

interface

uses
  Classes,
  sysutils,
  {$IFDEF INDY9}
  IdHash,
  {$ENDIF}
  {$IFDEF ACTIVEX}
  // Variants,
  {$ENDIF}
  {$IFDEF WIN32}
  windows;
  {$ENDIF}
  {$IFDEF LINUX}
   QForms;
  {$ENDIF}

type

   TRC4Data= record
    Key: array[0..255] of byte;         { current key }
    OrgKey: array[0..255] of byte;      { original key }
  end;

  TRC4 = class(TObject)
    private
      FData: TRC4Data;
      procedure RC4Init(var Data: TRC4Data; Key: pointer; Len: integer);
      procedure RC4Burn(var Data: TRC4Data);
      procedure RC4Crypt(var Data: TRC4Data; InData, OutData: pointer; Len: integer);
      procedure RC4Reset(var Data: TRC4Data);
    public
      constructor Create(EncryptionKey: string);
      procedure EncryptStream(instream,outstream: TMemoryStream);
      function EncryptString(value: string):string;
      procedure DecryptStream(instream,outstream: TMemoryStream);
      function DecryptString(value: string):string;
      procedure BurnKey;
   end;




 { xml-rpc data types }
  TRPCDataType = (rpNone,rpString,rpInteger,
                  rpBoolean,rpDouble,
                  rpDate,rpBase64,
                  rpStruct,rpArray,
                  rpName, rpError);

Function GetTempDir: String;
function FileIsExpired(sFileName: string; elapsed: integer): boolean;
function EncodeEntities(const sData: string):string;
function DecodeEntities(const sData: string):string;

function Replace(const sData: string; const sFind:
                 string; const sReplace: string): string;

function InStr(sStart: integer; const sData: string; const
               sFind: string): integer;

function Mid(const sData: string; nStart: integer): string;

function DateTimeToISO(const ConvertDate: TDateTime): string;

function ISOToDateTime(const ISOStringDate: string): TDateTime;

function ParseString(const SearchString: String;
                     const Delimiter: Char;
                     const Substrings: TStrings;
                     const AllowEmptyStrings: Boolean = False;
                     const ClearBeforeParse: Boolean = False): Integer;

function ParseStream(const SearchStream: TStream;
                     const Delimiter: Char;
                     const Substrings: TStrings;
                     const AllowEmptyStrings: Boolean = False;
                     const ClearBeforeParse: Boolean = False): Integer;
function FixEmptyString(value: String):String;
function URLEncode(Value : String) : String;
function StreamToString(const Stream: TStream): String;
procedure StringToStream(const Text: String;
                         const Stream: TMemoryStream);
{$IFDEF ACTIVEX}
function StreamToVariant( aStream: TStream): OleVariant;
procedure VariantToStream(const v: OleVariant; Stream: TStream);
{$ENDIF}
{$IFDEF INDY9}
function Hash128AsHex(const AHash128Value: T4x4LongWordRecord):String;
{$ENDIF}
const
   ValidURLChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$-_@.&+-!''*"(),;/#?:';

implementation

{------------------------------------------------------------------------------}
Function URLEncode(Value : String) : String;
var
 I : Integer;
begin
   Result := '';
   for I := 1 to Length(Value) do
      begin
         if Pos(UpperCase(Value[I]), ValidURLChars) > 0 then
            Result := Result + Value[I]
         else
            begin
               if Value[I] = ' ' then
                  Result := Result + '+'
               else
                  begin
                     Result := Result + '%';
                     Result := Result + IntToHex(Byte(Value[I]), 2);
                  end;
            end;
      end;
end;

{------------------------------------------------------------------------------}
function EncodeEntities(const sData: string):string;
begin
  result := StringReplace(sData,'&','&amp;',[rfReplaceAll]);
  result := StringReplace(result,'<','&lt;',[rfReplaceAll]);
  result := StringReplace(result,'>','&gt;',[rfReplaceAll]);
  result := StringReplace(result,'"','&quot;',[rfReplaceAll]);
  result := StringReplace(result,#39,'&apos;',[rfReplaceAll]);
end;

{------------------------------------------------------------------------------}
function DecodeEntities(const sData: string):string;
begin
  result := StringReplace(sData,'&lt;','<',[rfReplaceAll]);
  result := StringReplace(result,'&gt;','>',[rfReplaceAll]);
  result := StringReplace(result,'&quot;','"',[rfReplaceAll]);
  result := StringReplace(result,'&apos;',#39,[rfReplaceAll]);
  result := StringReplace(result,'&amp;','&',[rfReplaceAll]);
end;

{------------------------------------------------------------------------------}
{ String Parsing Routine                                                       }
{------------------------------------------------------------------------------}
function ParseString(const SearchString: String;
                     const Delimiter: Char;
                     const Substrings: TStrings;
                     const AllowEmptyStrings: Boolean;
                     const ClearBeforeParse: Boolean): Integer;

var
  Index: Integer;
  PrevCount: Integer;
  TempStr: String;

begin
  if (ClearBeforeParse) then
    SubStrings.Clear;

  PrevCount := Substrings.Count;

  { ensure that the last substring is found }
  TempStr := SearchString + Delimiter;

  while (Length(TempStr) > 0) do
    begin
    Index := Pos(Delimiter, TempStr);
    if ((Index > 1) or AllowEmptyStrings) then
      Substrings.Add(Copy(TempStr, 1, Index - 1));
    Delete(TempStr, 1, Index);
    end;

  Result := Substrings.Count - PrevCount;
end;

{------------------------------------------------------------------------------}
{ stream parser                                                                }
{------------------------------------------------------------------------------}
function ParseStream(const SearchStream: TStream;
                     const Delimiter: Char;
                     const Substrings: TStrings;
                     const AllowEmptyStrings: Boolean;
                     const ClearBeforeParse: Boolean): Integer;

begin
  Result := ParseString(StreamToString(SearchStream), Delimiter, SubStrings,
                        AllowEmptyStrings, ClearBeforeParse);
end;

{------------------------------------------------------------------------------}
{ convert stream to a string                                                   }
{------------------------------------------------------------------------------}
function StreamToString(const Stream: TStream): String;
begin
  Result := '';
  Stream.Seek(0, soFromBeginning);
  SetLength(Result, Stream.Size);
  Stream.Read(Result[1], Stream.Size);
end;

{------------------------------------------------------------------------------}
{  Converts a string to a stream                                               }
{------------------------------------------------------------------------------}
procedure StringToStream(const Text: String;
                         const Stream: TMemoryStream);
begin
  Stream.SetSize(Length(Text));
  Stream.Seek(0, soFromBeginning);
  Stream.Write(Text[1], Length(Text));
  Stream.Seek(0, soFromBeginning);
end;

{------------------------------------------------------------------------------}
{  Converts a date time to iso 8601 format                                     }
{------------------------------------------------------------------------------}
function DateTimeToISO(const ConvertDate: TDateTime): string;
begin
  result := FormatDateTime('yyyymmdd"T"hh:mm:ss',ConvertDate);
end;

{------------------------------------------------------------------------------}
{  Converts a ISO 8601 data to TDateTime                                       }
{------------------------------------------------------------------------------}
function ISOToDateTime(const ISOStringDate: string): TDateTime;
begin
  Result := EncodeDate(StrToInt(ISOStringDate[1] +
                                ISOStringDate[2] +
                                ISOStringDate[3] +
                                ISOStringDate[4]),

                                StrToInt(ISOStringDate[5] +
                                         ISOStringDate[6]),

                                StrToInt(ISOStringDate[7] +
                                         ISOStringDate[8])) +
                                EncodeTime(StrToInt(ISOStringDate[10] +
                                                    ISOStringDate[11]),

                                StrToInt(ISOStringDate[13] +
                                         ISOStringDate[14]),

                                StrToInt(ISOStringDate[16] +
                                            ISOStringDate[17]),0);

end;

{------------------------------------------------------------------------------}
{  Returns part of a string                                                    }
{------------------------------------------------------------------------------}
function Mid(const sData: string; nStart: integer): string;
begin
  Result := copy(sData, nStart, Length(sData) - (nStart - 1));
end;

{------------------------------------------------------------------------------}
{  Find position of string in sub string                                       }
{------------------------------------------------------------------------------}
function InStr(sStart: integer; const sData: string; const
               sFind: string): integer;
var
  c: integer;
label
  SkipFind;
begin
  c := sStart - 1;
  repeat
    if c > length(sData) then
  begin
    c := 0;
      goto SkipFind;
      end;
        inc(c);
      until copy(sData, c, length(sFind)) = sFind;
   SkipFind:
   Result := c;
 end;

{------------------------------------------------------------------------------}
{  replace item in string                                                      }
{------------------------------------------------------------------------------}
function Replace(const sData: string; const sFind:
                 string; const sReplace: string): string;
var
 c: integer;
 sTemp, sTemp2: string;
begin
  sTemp := sData;
  c := InStr(1, sTemp, sFind);
  while c <> 0 do
    begin
      sTemp2 := copy(sTemp, 1, c - 1) + sReplace + mid(sTemp, c + length(sFind));
      sTemp := sTemp2;
      c := InStr(c + length(sReplace), sTemp, sFind);
    end;
   result := sTemp;
end;

{------------------------------------------------------------------------------}
{Initialize the RC4 Engine                                                     }
{------------------------------------------------------------------------------}
procedure TRC4.RC4Init(var Data: TRC4Data; Key: pointer; Len: integer);
var
  xKey: array[0..255] of byte;
  i, j: integer;
  t: byte;
begin
  if (Len<= 0) or (Len> 256) then
    raise Exception.Create('RC4: Invalid key length');
  for i:= 0 to 255 do
  begin
    Data.Key[i]:= i;
    xKey[i]:= PByte(integer(Key)+(i mod Len))^;
  end;
  j:= 0;
  for i:= 0 to 255 do
  begin
    j:= (j+Data.Key[i]+xKey[i]) and $FF;
    t:= Data.Key[i];
    Data.Key[i]:= Data.Key[j];
    Data.Key[j]:= t;
  end;
  Move(Data.Key,Data.OrgKey,256);
end;

{------------------------------------------------------------------------------}
{Burn Key data from memory                                                     }
{------------------------------------------------------------------------------}
procedure TRC4.RC4Burn(var Data: TRC4Data);
begin
  FillChar(Data,Sizeof(Data),$FF);
end;

{------------------------------------------------------------------------------}
{Crypt and decrypt routine                                                     }
{------------------------------------------------------------------------------}
procedure TRC4.RC4Crypt(var Data: TRC4Data; InData, OutData: pointer; Len: integer);
var
  t, i, j: byte;
  k: integer;
begin
  i:= 0;
  j:= 0;
  for k:= 0 to Len-1 do
  begin
    i:= (i+1) and $FF;
    j:= (j+Data.Key[i]) and $FF;
    t:= Data.Key[i];
    Data.Key[i]:= Data.Key[j];
    Data.Key[j]:= t;
    t:= (Data.Key[i]+Data.Key[j]) and $FF;
    PByteArray(OutData)[k]:= PByteArray(InData)[k] xor Data.Key[t];
  end;
end;

{------------------------------------------------------------------------------}
{Reset the data keys                                                           }
{------------------------------------------------------------------------------}
procedure TRC4.RC4Reset(var Data: TRC4Data);
begin
  Move(Data.OrgKey,Data.Key,256);
end;

{------------------------------------------------------------------------------}
{Remove keys from memory                                                       }
{------------------------------------------------------------------------------}
procedure TRC4.BurnKey;
begin
  RC4Burn(FData);
end;

{------------------------------------------------------------------------------}
{Decrypt a memory stream                                                       }
{------------------------------------------------------------------------------}
procedure TRC4.DecryptStream(instream,outstream: TMemoryStream);
begin
  outstream.SetSize(instream.Size);
  RC4Crypt(FData,instream.Memory, outstream.Memory,instream.Size);
  RC4Reset(FData);
end;

{------------------------------------------------------------------------------}
{Secrypt a string value                                                        }
{------------------------------------------------------------------------------}
function TRC4.DecryptString(value: string): string;
var
 os: string;
begin
  SetLength(os,length(value));
  RC4Crypt(FData,pByteArray(value),pByteArray(os),length(os));
  result := os;
end;

{------------------------------------------------------------------------------}
{Encrypt stream data                                                           }
{------------------------------------------------------------------------------}
procedure TRC4.EncryptStream(instream,outstream: TMemoryStream);
begin
  outstream.SetSize(instream.Size);
  RC4Crypt(FData,instream.Memory, outstream.Memory,instream.Size);
  RC4Reset(FData);
end;

{------------------------------------------------------------------------------}
{Encrypt a string value                                                        }
{------------------------------------------------------------------------------}
function TRC4.EncryptString(value: string): string;
var
 os: string;
begin
  SetLength(os,length(value));
  RC4Crypt(FData,pByteArray(value),pByteArray(os),length(os));
  result := os;
  RC4Reset(FData);
end;

{------------------------------------------------------------------------------}
constructor TRC4.Create(EncryptionKey: string);
begin
  {initialize encryption engine}
  RC4Init(FData,pByteArray(EncryptionKey),length(EncryptionKey));
end;

{------------------------------------------------------------------------------}
//check a file to see if the elapsed time is expired
function FileIsExpired(sFileName: string; elapsed: integer): boolean;
var                                                                             
  FHandle: integer;
  FDate: TDateTime;
  ts: TTimeStamp;
  ct: TTimeStamp;
  mn: integer;
begin
  FHandle := FileOpen(sFileName, 0);
  try
    FDate := FileDateToDateTime(FileGetDate(FHandle));
    ts := DateTimeToTimeStamp(FDate);
    ct := DateTimeToTimeStamp(Now);
    mn := round((ct.Time - ts.Time) / 60000);
    if (mn > elapsed) then
      result := true
    else
      result := false;
  finally
    FileClose(FHandle);
  end;
end;

{------------------------------------------------------------------------------}
function GetTempDir: String;
{$IFDEF WIN32}
var
 buf: array[0..MAX_PATH] of Char;
{$ENDIF}
begin
  {$IFDEF WIN32}
  GetTempPath( Sizeof(buf), buf );
  Result := buf;
  If Result[Length(Result)] <> '\' Then
    Result := Result + '\';
  {$ENDIF}
  {$IFDEF LINUX}
   Result := '/var/tmp/'
  {$ENDIF}

end;

{------------------------------------------------------------------------------}
{$IFDEF INDY9}
function Hash128AsHex(const AHash128Value: T4x4LongWordRecord):string;
begin
  Result := IntToHex(AHash128Value[0],4) +
            IntToHex(AHash128Value[1],4) +
            IntToHex(AHash128Value[2],4) +
            IntToHex(AHash128Value[3],4);

end;
{$ENDIF}
{------------------------------------------------------------------------------}
function FixEmptyString(value: String):String;
begin
  result := StringReplace(value,'<string></string>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<string></nil></string>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<string></null></string>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<string> </string>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);

  result := StringReplace(result,'<value></value>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<value></nil></value>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<value></null></value>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(result,'<value> </value>','<string>[NULL]</string>',[rfReplaceAll,rfIgnoreCase]);
end;
{$IFDEF ACTIVEX}
function StreamToVariant( aStream: TStream): OleVariant;
var
 v: OleVariant;
 P  : pointer;
begin
   V := VarArrayCreate([0,aStream.Size -1],varByte);
   aStream.Position := 0;

   P:= VarArrayLock(V);
   try
     aStream.Read( P^, aStream.Size );
   finally
     VarArrayUnlock( V );
   end;
  result := V;
 End;

 procedure VariantToStream(const v: OleVariant; Stream: TStream);
  var
   P: pointer;
  begin
   Stream.Position := 0;
   Stream.Size := VarArrayHighBound(v,1) - VarArrayLowBound(v,1) + 1;
   P := VarArrayLock(v);
   Stream.Write(P^,Stream.Size);
   VarArrayUnlock(v);
   Stream.Position := 0;
  end;
{$ENDIF}
end.
