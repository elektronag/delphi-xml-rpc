
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/delphixml-rpc/tests/userlandtime/Unit2.pas,v 1.1.1.1 2003-11-19 22:12:23 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  xmlrpctypes, xmlrpcclient;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.Button1Click(Sender: TObject);
var
  caller: TCaller;
  func: TFunction;
  rslt: TResult;
begin
  caller := TCaller.Create;
  //caller.ProxyName := '192.168.0.1';
 // caller.ProxyPort := 9090;
  caller.EndPoint := '/RPC2';
  caller.HostName := 'time.xmlrpc.com';
  caller.HostPort := 80;
  

  func := TFunction.Create;
  func.ObjectMethod := 'currentTime.getCurrentTime';
  memo1.Lines.Add('executing call');
  rslt := caller.Execute(func);
  if (rslt.IsError) then
    begin
      memo1.Lines.Add('rpc call failed');
      memo1.Lines.Add('Error Code:' + IntToStr(rslt.getErrorCode));
      memo1.Lines.Add('Error Message' + rslt.GetErrorString);
    end
  else
    begin

      memo1.Lines.Add('rpc call test passed');
      memo1.Lines.Add('value:' +  DateTimeToStr(rslt.GetDate));
    end;
  func.Free;
  rslt.Free;
  caller.Free;
end;
end.
