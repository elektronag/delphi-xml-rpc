
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/demos/delphi/client/threaded/hello.pas,v 1.1.1.1 2003-11-19 22:11:34 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit hello;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, xmlrpctypes, xmlrpcclient, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ebMessage: TEdit;
    lstMessages: TListBox;
    btnMany: TButton;
    ebHelloCount: TEdit;
    udHelloCount: TUpDown;
    ebPort: TEdit;
    udPort: TUpDown;
    ebHost: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure btnManyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FCaller: TCaller;
    FFunc : TFunction;
    procedure AddMessage(const Msg : string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.AddMessage(const Msg: string);
begin
  lstMessages.Items.Add(Msg);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  rst  : TResult;
begin
  { A function and caller object is used to perform this
    miracle. Set the host name and port and the endpoint
    to be called. We then set the function name to be called
    and add a single string parameter. The function object is
    passed to the execute method and a tresult object is returned}

  FCaller.HostName := ebHost.Text;
  FCaller.HostPort := udPort.Position;
  FCaller.EndPoint := '/RPC2';

  FFunc.Clear;
  FFunc.ObjectMethod := 'example.getHello';
  FFunc.AddParam(ebMessage.Text);
  AddMessage('Starting call');

  try
    rst := FCaller.Execute(FFunc);
    if rst.IsError then
      Addmessage('Error:' + rst.GetErrorString())
    else
      AddMessage(rst.GetString());
  except
    on E : Exception do
      AddMessage(StringReplace(E.Message, #13#10, ': ', [rfReplaceAll]) );
  end;


end;

procedure TForm1.btnManyClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to udHelloCount.Position - 1 do
    Button1Click(self);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FFunc := TFunction.Create;
  FCaller := TCaller.Create;
end;

end.
