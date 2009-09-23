
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/demos/delphi/server/simple/server.pas,v 1.1.1.1 2003-11-19 22:11:43 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit server;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, xmlrpcserver,xmlrpctypes;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    { exposed delphi method }
    procedure HelloMethod(const MethodName: string; const plist: TList; const return: TReturn);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
FServer:TServer;
FMethodHandler: TMethodHandler;
begin
  FServer := TServer.Create;
  FServer.ListenPort := 80;
  FServer.EnableIntrospect := true;
  FMethodHandler := TMethodHandler.Create;
  FMethodHandler.Name := 'example.getHello';
  FMethodHandler.Method := HelloMethod;
  FMethodHandler.Signature := 'string (string myval)';
  FMethodHandler.Help := 'Just a simple hello world example method';
  FServer.RegisterMethodHandler(FMethodHandler);
  FServer.Active := True;
  showmessage('xml-rpc hello server has been started');

end;

procedure TForm1.HelloMethod(const MethodName: string; const plist: TList; const return: TReturn);
var
  szSent: string;
begin
{The parameter list is sent to your method as a TList of parameters
 this must be casted to a parameter to be accessed. If a error occurs
 during the execution of your method the server will fall back to a global
 handler and try to recover in which case the stack error will be sent to
 the client}

 {grab the sent string}
 szSent := TParameter(plist[0]).GetString;

 {return a message showing what was sent}
  return.AddParam('You just sent ' + szSent);
end;

end.
