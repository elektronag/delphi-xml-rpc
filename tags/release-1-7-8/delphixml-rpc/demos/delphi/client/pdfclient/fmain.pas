
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/demos/delphi/client/pdfclient/fmain.pas,v 1.1.1.1 2003-11-19 22:11:42 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit fmain;

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,xmlrpcclient,xmlrpctypes;

type
  TfmMain = class(TForm)
    memText: TMemo;
    btnLoad: TButton;
    btnConvert: TButton;
    odOpen: TOpenDialog;
    sdSave: TSaveDialog;
    procedure btnLoadClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btnLoadClick(Sender: TObject);
begin
  if odOpen.Execute then
    memText.Lines.LoadFromFile(odOpen.FileName);
end;

procedure TfmMain.btnConvertClick(Sender: TObject);
var
  FCaller: TCaller;
  FReturn: TResult;
  FFunc: TFunction;
begin
  FCaller := TCaller.Create;
  FFunc := TFunction.Create;
  FCaller.HostName := '192.168.0.4';
  FCaller.HostPort := 8012;
  FCaller.EndPoint := '/RPC2';
  FFunc.ObjectMethod := 'conversion.text2pdf';
  FFunc.AddParamBase64String(memText.Text);
  FReturn := FCaller.Execute(FFunc);
  {if we get a error display it}
  if FReturn.IsError then
    begin
      showmessage(FReturn.getErrorString);
      FFunc.Free;
      FCaller.Free;
      exit;
    end;

  {save the file}
  sdSave.FileName := 'converted.pdf';
  if sdSave.Execute then
    FReturn.putBase64ToFile(sdSave.FileName);

  FFunc.Free;
  FCaller.Free;
end;

end.
