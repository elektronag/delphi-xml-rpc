
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/demos/kylix/server/pdfdaemon/rpcserver.pas,v 1.1.1.1 2003-11-19 22:11:25 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit rpcserver;

interface
uses
Libc,xmlrpcserver,xmlrpctypes,xmlrpccommon,
Classes,SysUtils,IdHash,IdHashMessageDigest;

type
  TRPCServer = class(TObject)
  private
  FServer: TServer;
  public
    constructor Create;
    procedure conversion_text2pdf(const plist: TList; const return: TReturn);
    function CreateFileHash(data: TMemoryStream): string;
  end;

implementation

constructor TRPCServer.Create;
var
  mthd: TMethodHandler;
begin
  FServer := TServer.Create;
  FServer.ListenPort := 8012;
  mthd := TMethodHandler.Create;
  mthd.Name := 'conversion.text2pdf';
  mthd.Method := conversion_text2pdf;
  mthd.Signature := 'Base64 File (Base64 StringData)';
  mthd.Help := 'This method takes text data as a base64 type' + #10 +
               'and converts it to a adobe pdf document.';
  FServer.RegisterMethodHandler(mthd);
  FServer.Active := true;
end;

{create a hash value for this stream}
function TRPCServer.CreateFileHash(data: TMemoryStream): string;
var
 md : TIdHashMessageDigest5;
begin
  md := TIdHashMessageDigest5.Create;
  result := Hash128AsHex(md.HashValue(data));
  md.Free;
end;

procedure TRPCServer.conversion_text2pdf(const plist: TList; const return: TReturn);
var
  szFileName : string;
  storage: string;
begin
  storage := '/opt/';
  if not (plist.Count = 1) then
    begin
      return.AddError(101,'You sent not enough or to many parameters');
      exit;
    end;

  if not TParameter(plist[0]).IsBase64 then
    begin
      return.AddError(102,'The sent parameter was not a base64 type');
      exit;
    end;
  {create a hash for the request}
  szFileName := CreateFileHash(TParameter(plist[0]).GetBase64Stream);

  {if we have the cached copy send it}
  if (FileExists(storage + szFileName + '.pdf')) then
    begin
      return.AddParamBase64File(storage + szFileName + '.pdf');
      exit;
    end;
  {save the request to storage}
  TParameter(plist[0]).PutBase64ToFile(storage + szFileName + '.txt');

  {lets check to see if the file made it to storage}
  if not(FileExists(storage + szFileName + '.txt')) then
    begin
      return.AddError(103,'Unable to write sent file to storage.');
    end;

  {convert document to postscript}
  if Libc.system(pchar('a2ps -R --output=' + storage + szFileName + '.ps ' +
                        storage + szFileName + '.txt')) = -1 then
    begin
      return.AddError(104,'Unable to convert sent file to postscript');
      exit;
    end;

  {convert file to adobe pdf}
  if Libc.system(pchar('ps2pdf ' + storage + szFileName + '.ps ' + storage +
                 szFileName + '.pdf')) = -1 then
     begin
       return.AddError(105,'Unable to convert postscript file to pdf');
     end;

  {if we have a pdf document in storage send it}
  if FileExists(storage + szFileName + '.pdf') then
   return.AddParamBase64File(storage + szFileName + '.pdf')
  else
   return.AddError(106,'Unable to find converted pdf file in storage');

end;

end.
