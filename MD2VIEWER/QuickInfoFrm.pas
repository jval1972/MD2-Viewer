//------------------------------------------------------------------------------
//
//  MD2-Viewer
//  Copyright (C) 2004 - 2018 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Quick Info Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Old Site: http://www.geocities.ws/jimmyvalavanis/applications/md2viewer.html
//  New Site: https://sourceforge.net/projects/md2-viewer/
//------------------------------------------------------------------------------

{$I defs.inc}

unit QuickInfoFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TQuickInfoForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OKBtn: TButton;
    Bevel1: TBevel;
    Panel3: TPanel;
    Memo1: TMemo;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    procedure Label2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  QuickInfoForm: TQuickInfoForm;

implementation

{$R *.DFM}

uses
  ShellApi, md2v_globals;

procedure TQuickInfoForm.Label2Click(Sender: TObject);
begin
  ShellExecute(
    handle,
      PChar(rsOpen),
        PChar(rsHomePage),
          nil, nil, SW_SHOWNORMAL);
end;

end.
