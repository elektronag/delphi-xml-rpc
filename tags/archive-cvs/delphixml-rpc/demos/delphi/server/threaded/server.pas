
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/demos/delphi/server/threaded/server.pas,v 1.1.1.1 2003-11-19 22:11:44 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit server;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  ExtCtrls,
  xmlrpcserver, xmlrpctypes,        { XML stuff}
  xmlrpcsynchclasses,                { synch class }
  xmlrpcthreadallocator, ComCtrls;  { allocator class}

type
  TForm1 = class(TForm)
    Button1: TButton;
    lstMessages: TListBox;
    rgrpThreadSynch: TRadioGroup;
    ebPort: TEdit;
    udPort: TUpDown;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ebPortChange(Sender: TObject);
  private
    { Private declarations }
    FServer : TServer;
    FMethodHandler: TMethodHandler;
    FSynchroniser : TSynchroniser;
    Flist: TList;
    FReturn: TReturn;
    FAllocator : TThreadAllocator;
    procedure AddMessage(const Msg : string);
    procedure HelloMethod2(const MethodName: string; const plist: TList; const return: TReturn);
    procedure DoHelloMethod(Sender: TObject);
    function AllocateStringList : TObject;
  public
    { Public declarations }
    { exposed delphi method }
    procedure HelloMethod(const MethodName: string; const plist: TList; const return: TReturn);
  end;

var
  Form1: TForm1;

implementation


{$R *.DFM}

procedure TForm1.AddMessage(const Msg: string);
begin
  if lstMessages.Items.Count > 100 then
    lstMessages.Clear;
  lstMessages.Items.Add(IntToStr(GetCurrentThreadId) + ' ' + Msg);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  
  if Fserver = nil then
    FServer := TServer.Create;
  if not FServer.Active then
  begin
    FServer.ListenPort := udPort.Position;
    if FMethodHandler = nil then
    begin
      FMethodHandler := TMethodHandler.Create;
      FMethodHandler.Name := 'example.getHello';
      FServer.RegisterMethodHandler(FMethodHandler);
    end;

    if rgrpThreadSynch.ItemIndex = 0 then
      FMethodHandler.Method := HelloMethod
    else
    begin
      FMethodHandler.Method := HelloMethod2;
      FSynchroniser.Method := DoHelloMethod;
    end;

    FServer.Active := true;
    Button1.Caption := 'Stop Server';
    AddMessage('xml-rpc hello server has been started');
  end
  else
  begin
    FServer.Active := false;
    Button1.Caption := 'Start Server';
    AddMessage('xml-rpc hello server has been stopped');

  end;
  rgrpThreadSynch.Enabled := not FServer.Active;

end;

procedure TForm1.HelloMethod(const MethodName: string; const plist: TList; const return: TReturn);
var
  szSent: string;
  StringList : TStringList;
begin
  {The parameter list is sent to your method as a TList of parameters
  this must be casted to a parameter to be accessed. If a error occurs
  during the execution of your method the server will fall back to a global
  handler and try to recover in which case the stack error will be sent to
  the client}

  {grab the sent string}
  szSent := TParameter(plist[0]).GetString;
  AddMessage('Sent ' + szSent);

  {return a message showing what was sent}
  return.AddParam('You just sent ' + szSent);

  { test creation and retrieval of per - thread objects }

  StringList := TStringList(FAllocator.ThreadObject);
  StringList.Values['Usage Count'] :=
    IntToStr(StrToIntDef(StringList.Values['Usage Count'], 0) + 1);
  AddMessage('StringList Usage Count ' + StringList.Values['Usage Count']);


end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  FSynchroniser := TSynchroniser.Create;
  FAllocator := TThreadAllocator.Create;
  FAllocator.CreationProc := AllocateStringList;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

  FAllocator.Free;
  fServer.Free;
  FMethodHandler.Free;
  FSynchroniser.Free;

end;

procedure TForm1.HelloMethod2(const MethodName: string; const plist: TList; const return: TReturn);
begin
  FList := plist;
  FReturn := return;
  FSynchroniser.RunMethod(self);
  fLIst := nil;
  FReturn := nil;
end;

procedure TForm1.DoHelloMethod(Sender: TObject);
begin
  HelloMethod('',Flist, FReturn);
end;

function TForm1.AllocateStringList: TObject;
begin
  Result := TStringList.Create;
end;

procedure TForm1.ebPortChange(Sender: TObject);
var
  Port : integer;
begin
  Port := StrToIntDef(ebPort.Text, 80);
  if ((Port > 65535) or (Port < 0)) then
   ebPort.Text := '80';
end;

end.
