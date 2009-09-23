
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/demos/kylix/client/simple/CLXclient.pas,v 1.1.1.1 2003-11-19 22:11:24 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit CLXclient;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls,
  xmlrpcclient, xmlrpctypes
  ;

type
  TForm3 = class(TForm)
    Button1: TButton;
    ebMessage: TEdit;
    lstMessages: TListBox;
    btnMany: TButton;
    speHelloCount: TSpinEdit;
    ebHost: TEdit;
    sePort: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnManyClick(Sender: TObject);
  private
    { Private declarations }
    { Private declarations }
    FCaller: TCaller;
    FFunc : TFunction;
    procedure AddMessage(const Msg : string);
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation


{$R *.xfm}

procedure TForm3.AddMessage(const Msg: string);
begin
  lstMessages.Items.Add(Msg);
 end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  FFunc := TFunction.Create;
  FCaller := TCaller.Create;
  sePort.Value := 1025;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  if Assigned(fFunc) then
    FFunc.Free;
  if Assigned(FCaller) then
    FCaller.Free;
end;

procedure TForm3.Button1Click(Sender: TObject);
var
  rst  : TResult;
begin
  { A function and caller object is used to perform this
    miracle. Set the host name and port and the endpoint
    to be called. We then set the function name to be called
    and add a single string parameter. The function object is
    passed to the execute method and a tresult object is returned}

  FCaller.HostName := ebHost.Text;
  FCaller.HostPort := sePort.Value;
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

procedure TForm3.btnManyClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to speHelloCount.Value - 1 do
    Button1Click(self);

end;

end.
