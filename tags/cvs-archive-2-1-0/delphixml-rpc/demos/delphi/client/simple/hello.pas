
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/demos/delphi/client/simple/hello.pas,v 1.1.1.1 2003-11-19 22:11:27 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit hello;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, xmlrpctypes, xmlrpcclient;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  caller: TCaller;
  func : TFunction;
  rst  : TResult;
  index: integer;

begin
  { A function and caller object is used to perform this
    miracle. Set the host name and port and the endpoint
    to be called. We then set the function name to be called
    and add a single string parameter. The function object is
    passed to the execute method and a tresult object is returned}


  func := TFunction.Create;
  caller := TCaller.Create;
  caller.HostName := '192.168.0.3';
  caller.HostPort := 80;
  caller.EndPoint := '/hailstone/server.php';

  func.ObjectMethod := 'caps.ping';
      func.AddParam('fuckers');
    
       //for index := 0 to 2 do
  // begin
     rst := caller.Execute(func);
   //  index := index + 1;
 //  end;

  if rst.IsError then
    showmessage('Error:' + rst.GetErrorString())
  else
    ShowMessage('success');

  func.Free;
  rst.Free;
  caller.Free;
end;

end.
