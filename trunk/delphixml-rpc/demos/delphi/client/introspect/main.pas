
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/demos/delphi/client/introspect/main.pas,v 1.1.1.1 2003-11-19 22:11:28 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,xmlrpcclient,xmlrpctypes;

type
  TfmMain = class(TForm)
    gbConnection: TGroupBox;
    gbMethods: TGroupBox;
    gbSignature: TGroupBox;
    gbHelpText: TGroupBox;
    lbHost: TLabel;
    edHost: TEdit;
    lbPort: TLabel;
    edPort: TEdit;
    btnConnect: TButton;
    lbEndPoint: TLabel;
    edEndPoint: TEdit;
    lbProxyHost: TLabel;
    edProxyHost: TEdit;
    Label1: TLabel;
    edProxyPort: TEdit;
    lbMethod: TListBox;
    memSignature: TMemo;
    memHelp: TMemo;
    procedure btnConnectClick(Sender: TObject);
    procedure lbMethodClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btnConnectClick(Sender: TObject);
var
  caller: TCaller;
  rtn: TResult;
  func: TFunction;
  ar : TArray;
  index: integer;
begin
  lbMethod.Clear;
  {check host name}
  if trim(edHost.Text) = '' then
    begin
      showmessage('Host name is required');
      exit;
    end;

  {check port}
  if trim(edPort.Text) = '' then
    begin
      showmessage('Port is required');
      exit;
    end;

  {check end point}
  if trim(edEndPoint.Text) = '' then
    begin
      showmessage('End Point is required');
      exit;
    end;

  caller := TCaller.Create;
  caller.HostName := trim(edHost.text);
  caller.HostPort := StrToInt(trim(edPort.text));
  caller.EndPoint := trim(edEndPoint.text);
  if trim(edProxyHost.text) <> '' then
    begin
      caller.ProxyName := trim(edProxyHost.text);
      caller.ProxyPort := StrToInt(trim(edProxyPort.text));
    end;

  func := TFunction.Create;
  func.ObjectMethod := 'system.listMethods';
  rtn := caller.Execute(func);
  if rtn.IsError then
    begin
      showmessage('Error:' + rtn.GetErrorString);
      exit;
    end;

  if rtn.IsArray then
    begin
      ar := rtn.GetArray;
      for index := 0 to ar.Count -1 do
        lbMethod.Items.Add(ar.GetString(index));

    end;

end;

procedure TfmMain.lbMethodClick(Sender: TObject);
var
  caller: TCaller;
  rtn: TResult;
  func: TFunction;
  ar : TArray;
  mr : TArray;
  index: integer;

begin
 memSignature.Clear;
 memHelp.Clear;
 if lbMethod.Items.Count > 0 then
   begin

     {check host name}
     if trim(edHost.Text) = '' then
       begin
         showmessage('Host name is required');
         exit;
       end;

    {check port}
    if trim(edPort.Text) = '' then
      begin
        showmessage('Port is required');
        exit;
     end;

    {check end point}
    if trim(edEndPoint.Text) = '' then
      begin
        showmessage('End Point is required');
        exit;
      end;

    caller := TCaller.Create;
    caller.HostName := trim(edHost.text);
    caller.HostPort := StrToInt(trim(edPort.text));
    caller.EndPoint := trim(edEndPoint.text);
    if trim(edProxyHost.text) <> '' then
      begin
        caller.ProxyName := trim(edProxyHost.text);
        caller.ProxyPort := StrToInt(trim(edProxyPort.text));
      end;

    func := TFunction.Create;
    func.ObjectMethod := 'system.methodSignature';
    func.AddParam(lbMethod.Items[lbMethod.ItemIndex]);
    rtn := caller.Execute(func);
    if rtn.IsError then
      begin
        showmessage('Error:' + rtn.GetErrorString);
        exit;
      end;

  if rtn.IsArray then
    begin
      ar := rtn.GetArray;
      mr := ar.GetArray(0);
      for index := 0 to mr.Count -1 do
        memSignature.Lines.Add(mr.GetString(index));
      end;

   {clear the function and grab help text}
   func.Clear;
   func := TFunction.Create;
   func.ObjectMethod := 'system.methodHelp';
   func.AddParam(lbMethod.Items[lbMethod.ItemIndex]);
   rtn := caller.Execute(func);
   if rtn.IsError then
     begin
       showmessage('Error:' + rtn.GetErrorString);
       exit;
     end;
   if rtn.IsString then
      memHelp.Text := rtn.GetString;

 end;

end;

end.
