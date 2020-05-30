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
//  Open MD2 Model Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Old Site: http://www.geocities.ws/jimmyvalavanis/applications/md2viewer.html
//  New Site: https://sourceforge.net/projects/md2-viewer/
//------------------------------------------------------------------------------

{$I defs.inc}

unit OpenMD2Frm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ExtDlgs, Menus, ComCtrls, Buttons,
  se_D3DUtils, se_DirectX, se_Main,
  MessageBox, AnotherReg, Validations, ValCtrls, rmBaseEdit,
  rmBtnEdit, xM8, xPPM, xTGA, zBitmap, JPEG, xGIF, pcximage, dibimage, pngimage;

type
  TOpenMD2Form = class(TForm)
    Bevel2: TBevel;
    Panel1: TPanel;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label4: TLabel;
    ComboBox1: TComboBox;
    Label14: TLabel;
    UseInterpolationCheckBox: TCheckBox;
    OpenDialog1: TOpenPictureDialog;
    OpenDialog2: TOpenDialog;
    Label3: TLabel;
    Label5: TLabel;
    ImagesPanel: TPanel;
    Image2: TImage;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OKBtnClick(Sender: TObject);
    procedure ModelEditBtn1Click(Sender: TObject);
    procedure ModelEditExit(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure TextureEditBtn1Click(Sender: TObject);
  private
    { Private declarations }
    ModelEdit: TrmBtnEdit;
    TextureEdit: TrmBtnEdit;
    AnimStartEdit: TIntegerEdit;
    AnimEndEdit: TIntegerEdit;
    AnimSpeedEdit: TFloatEdit;

    FormRestorer1: TFormRestorer;

    MessageBox1: TMessageBox;
    MessageBox2: TMessageBox;
    MessageBox3: TMessageBox;

    procedure UpdateBuildInMoves;
  public
    { Public declarations }
    Info: TD3DActorInfo;
    procedure SetInfo(inf: TD3DActorInfo);
    procedure GetInfo(var inf: TD3DActorInfo);
  end;

function QueryImportQuakeModel(var inf: TD3DActorInfo; var moves: TStringList; var MoveIndex: integer): boolean;

implementation

uses
  se_DXDsngUtils, se_Quake2Utils, se_DXModels, Unit1;

{$R *.DFM}

function QueryImportQuakeModel(var inf: TD3DActorInfo; var moves: TStringList; var MoveIndex: integer): boolean;
begin
  result := false;
  with TOpenMD2Form.Create(nil) do
  try
    SetInfo(inf);
    ShowModal;
    if ModalResult = mrOK then
    begin
      GetInfo(inf);
      if moves = nil then
        moves := TStringList.Create;
      moves.Clear;
      moves.AddStrings(ComboBox1.Items);
      MoveIndex := ComboBox1.ItemIndex;
      result := true;
    end;
  finally
    Free;
  end;
end;

procedure TOpenMD2Form.FormCreate(Sender: TObject);
var
  i: integer;
begin
  ModelEdit := TrmBtnEdit.Create(self);
  ModelEdit.Parent := self;
  ModelEdit.Left := 16;
  ModelEdit.Top := 38;
  ModelEdit.Width := 289;
  ModelEdit.Height := 21;
  ModelEdit.Hint := 'MD2 Model file';
  ModelEdit.BtnWidth := 22;
  ModelEdit.Btn1Glyph := Image1.Picture.Bitmap;
  ModelEdit.Btn1NumGlyphs := 1;
  ModelEdit.Btn2Glyph := Image2.Picture.Bitmap;
  ModelEdit.Btn2NumGlyphs := 1;
  ModelEdit.TabOrder := 0;
  ModelEdit.OnExit := ModelEditExit;
  ModelEdit.OnBtn1Click := ModelEditBtn1Click;
  Label1.FocusControl := ModelEdit;

  TextureEdit := TrmBtnEdit.Create(self);
  TextureEdit.Parent := self;
  TextureEdit.Left := 16;
  TextureEdit.Top := 86;
  TextureEdit.Width := 289;
  TextureEdit.Height := 21;
  TextureEdit.Hint := 'Texture file';
  TextureEdit.BtnWidth := 22;
  TextureEdit.Btn1Glyph := Image1.Picture.Bitmap;
  TextureEdit.Btn1NumGlyphs := 1;
  TextureEdit.Btn2Glyph := Image2.Picture.Bitmap;
  TextureEdit.Btn2NumGlyphs := 1;
  TextureEdit.TabOrder := 1;
  TextureEdit.OnBtn1Click := TextureEditBtn1Click;
  Label3.FocusControl := TextureEdit;

  AnimStartEdit := TIntegerEdit.Create(self);
  AnimStartEdit.Parent := GroupBox3;
  AnimStartEdit.Left := 88;
  AnimStartEdit.Top := 52;
  AnimStartEdit.Width := 49;
  AnimStartEdit.Height := 21;
  AnimStartEdit.Hint := 'From this frame';
  AnimStartEdit.MaxValue := 65535;
  AnimStartEdit.Value := 0;
  AnimStartEdit.TabOrder := 1;
  AnimStartEdit.Text := '0';
  Label2.FocusControl := AnimStartEdit;

  AnimEndEdit := TIntegerEdit.Create(self);
  AnimEndEdit.Parent := GroupBox3;
  AnimEndEdit.Left := 240;
  AnimEndEdit.Top := 52;
  AnimEndEdit.Width := 49;
  AnimEndEdit.Height := 21;
  AnimEndEdit.Hint := 'To that frame';
  AnimEndEdit.MaxValue := 65535;
  AnimEndEdit.Value := 0;
  AnimEndEdit.TabOrder := 2;
  AnimEndEdit.Text := '0';
  Label4.FocusControl := AnimEndEdit;

  AnimSpeedEdit := TFloatEdit.Create(self);
  AnimSpeedEdit.Parent := GroupBox3;
  AnimSpeedEdit.Left := 240;
  AnimSpeedEdit.Top := 84;
  AnimSpeedEdit.Width := 49;
  AnimSpeedEdit.Height := 21;
  AnimSpeedEdit.Hint := 'Animation duration in seconds';
  AnimSpeedEdit.MaxValue := 65535;
  AnimSpeedEdit.TabOrder := 3;
  AnimSpeedEdit.Text := '0';
  Label5.FocusControl := AnimSpeedEdit;

  FormRestorer1 := TFormRestorer.Create(self);
  FormRestorer1.ParentKey := DXViewerForm.AppConfigKey1;
  FormRestorer1.Name := 'FormRestorer1';
  FormRestorer1.Restoring := frPositionOnly;
  FormRestorer1.Restore;

  MessageBox1 := TMessageBox.Create(self);
  MessageBox1.Caption := 'MD2 Model Viewer';
  MessageBox1.Text := 'You must provide a texture name as an existing file on disk';
  MessageBox1.Buttons := mbxYesNo;
  MessageBox1.Icon := mbxIconQuestion;
  MessageBox1.DefaultButton := mbxDefButton1;
  MessageBox1.Modality := mbxDefModality;
  MessageBox1.TextAlignment := mbxLeft;

  MessageBox2 := TMessageBox.Create(self);
  MessageBox2.Caption := 'MD2 Model Viewer';
  MessageBox2.Text := 'You must provide an MD2 model as an existing model on disk!';
  MessageBox2.Buttons := mbxOK;
  MessageBox2.Icon := mbxIconStop;
  MessageBox2.DefaultButton := mbxDefButton1;
  MessageBox2.Modality := mbxDefModality;
  MessageBox2.TextAlignment := mbxLeft;

  MessageBox3 := TMessageBox.Create(self);
  MessageBox3.Caption := 'MD2 Model Viewer';
  MessageBox3.Text := 'Unknown model type!';
  MessageBox3.Buttons := mbxOK;
  MessageBox3.Icon := mbxIconStop;
  MessageBox3.DefaultButton := mbxDefButton1;
  MessageBox3.Modality := mbxDefModality;
  MessageBox3.TextAlignment := mbxLeft;

  FillChar(Info, SizeOf(Info),Chr(0));
  for i := 0 to MAXTEXTURES - 1 do
  begin
    Info.TextureNames[i, 0] := '';
    Info.TextureNames[i, 1] := '';
  end;

  Info.Key := GenGlobalID;
  Info.zOrder := 500;
  Info.Interval := 1.0;
  Info.C := clWhite;
  Info.Scale := 0.1;
  Info.AnimStart := 0;
  Info.AnimEnd := 1;
  Info.AnimSpeed := 1.0;
  Info.UseInterpolation := true;
  Info.MinFramesPerSec := 100;
  Info.HideDistance := 1000000.0;
  Info.UpdateDistance := 500000.0;

  Info.PathToModel := '';

  SetInfo(Info);
end;

procedure TOpenMD2Form.SetInfo(inf: TD3DActorInfo);
begin
  Info := inf;
  ModelEdit.Text := Info.PathToModel;
  AnimStartEdit.Value := Info.AnimStart;
  AnimEndEdit.Value := Info.AnimEnd;
  AnimSpeedEdit.Value := Info.AnimSpeed;

  UseInterpolationCheckBox.Checked := Info.UseInterpolation;
  if Info.UseInterpolation then
    Info.MinFramesPerSec := 100
  else
    Info.MinFramesPerSec := 5;

  TextureEdit.Text := Info.TextureNames[0, 0];

  UpdateBuildInMoves;
end;

procedure TOpenMD2Form.GetInfo(var inf: TD3DActorInfo);
begin
// default values...
  Info.Key := 0;
  Info.zOrder := 100;
  Info.Interval := 0;
  Info.x := 0;
  Info.y := 0;
  Info.z := 4.0;
  Info.dx := 0;
  Info.dy := pi;
  Info.dz := 0;
  Info.HideDistance := 1000000.0;
  Info.UpdateDistance := 10000.0;
  Info.Scale := 0.1;
  Info.C := clWhite;

  Info.PathToModel := ModelEdit.Text;
  Info.AnimStart := AnimStartEdit.Value;
  Info.AnimEnd := AnimEndEdit.Value;
{  if AnimSpeedEdit.Value <= 0 then
    AnimSpeedEdit.Value := abs(Info.AnimEnd - Info.AnimStart) / 5;}
  Info.AnimSpeed := AnimSpeedEdit.Value;
  Info.TextureNames[0, 0] := TextureEdit.Text;
  Info.UseInterpolation := UseInterpolationCheckBox.Checked;

  if Info.UseInterpolation then
    Info.MinFramesPerSec := 100
  else
    Info.MinFramesPerSec := 5;

  inf := Info;
end;

procedure TOpenMD2Form.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  GetInfo(Info);
end;

procedure TOpenMD2Form.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    if (ModelEdit.Text = '') or not FileExists(ModelEdit.Text) then
    begin
      MessageBox2.Execute;
      CanClose := false;
      exit;
    end;
    
    if (UpperCase(ExtractFileExt(ModelEdit.Text)) <> UpperCase(rsExtMD2)) and
{$IFNDEF NO_MODELX}
       (UpperCase(ExtractFileExt(ModelEdit.Text)) <> UpperCase(rsExtX)) and
{$ENDIF}
       (UpperCase(ExtractFileExt(ModelEdit.Text)) <> UpperCase(rsExtVRT)) then
    begin
      MessageBox3.Execute;
      CanClose := false;
      exit;
    end;

    if Info.TextureNames[0, 0] <> '' then
      CanClose := true
    else
    begin
      MessageBox1.Execute;
      CanClose := MessageBox1.ReturnValue = IDYES;
    end;
  end
  else
    CanClose := true
end;

procedure TOpenMD2Form.OKBtnClick(Sender: TObject);
begin
  GetInfo(Info);
end;

procedure TOpenMD2Form.ModelEditBtn1Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
  begin
    ModelEdit.Text := OpenDialog2.FileName;
    UpdateBuildInMoves;
    if ComboBox1.Items.Count > 0 then
    begin
      ComboBox1.ItemIndex := 0;
      ComboBox1Click(Sender);
    end;
  end;
end;

procedure TOpenMD2Form.UpdateBuildInMoves;
var f: TFileStream;
    mdl: TModel;
    oldMove: string;
begin
  if ComboBox1.ItemIndex > -1 then
    oldMove := ComboBox1.Items[ComboBox1.ItemIndex]
  else
    oldMove := EmptyStr;
  ComboBox1.Items.Clear;
  UseInterpolationCheckBox.Enabled := false;
  if FileExists(ModelEdit.Text) then
  begin
    f := TFileStream.Create(ModelEdit.Text, fmOpenRead or fmShareDenyWrite);
    Screen.Cursor := crHourglass;
    try
      if UpperCase(ExtractFileExt(ModelEdit.Text)) = UpperCase(rsExtMD2) then
      begin
        mdl := TModelMD2.Create(ModelEdit.Text, f);
        try
          UseInterpolationCheckBox.Enabled := true;
          ComboBox1.Items.AddStrings(mdl.frameNames);
        finally
          mdl.Free;
        end;
      end
      else if UpperCase(ExtractFileExt(ModelEdit.Text)) = UpperCase(rsExtVRT) then
      begin
        mdl := TModelVRT.Create(ModelEdit.Text, f);
        try
          UseInterpolationCheckBox.Checked := false;
          ComboBox1.Items.AddStrings(mdl.frameNames);
        finally
          mdl.Free;
        end;
      end
{$IFNDEF NO_MODELX}
      else if UpperCase(ExtractFileExt(ModelEdit.Text)) = UpperCase(rsExtX) then
      begin
        mdl := TModelX.Create(ModelEdit.Text, f);
        try
          UseInterpolationCheckBox.Checked := false;
          ComboBox1.Items.AddStrings(mdl.frameNames);
        finally
          mdl.Free;
        end;
      end{$ENDIF};
    finally
      f.Free;
      Screen.Cursor := crDefault;
    end;
  end;
  if oldMove <> EmptyStr then
    ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(oldMove);
end;

procedure TOpenMD2Form.ModelEditExit(Sender: TObject);
begin
  UpdateBuildInMoves;
end;

procedure TOpenMD2Form.ComboBox1Click(Sender: TObject);
var f: TFileStream;
    mdl: TModel;
    oldMove: string;
    idx: integer;
begin
  if ComboBox1.ItemIndex > -1 then
  begin
    oldMove := ComboBox1.Items[ComboBox1.ItemIndex];
    if FileExists(ModelEdit.Text) then
    begin
      f := TFileStream.Create(ModelEdit.Text, fmOpenRead or fmShareDenyWrite);
      Screen.Cursor := crHourglass;
      try
        if UpperCase(ExtractFileExt(ModelEdit.Text)) = UpperCase(rsExtMD2) then
        begin
          mdl := TModelMD2.Create(ModelEdit.Text, f);
          try
            idx := mdl.frameNames.IndexOf(oldMove);
            if idx > -1 then
            begin
              AnimStartEdit.Value := mdl.StartFrame(idx);
              AnimEndEdit.Value := mdl.EndFrame(idx);
            end;
          finally
            mdl.Free;
          end;
        end
        else if UpperCase(ExtractFileExt(ModelEdit.Text)) = UpperCase(rsExtVRT) then
        begin
          mdl := TModelVRT.Create(ModelEdit.Text, f);
          try
            idx := mdl.frameNames.IndexOf(oldMove);
            if idx > -1 then
            begin
              AnimStartEdit.Value := mdl.StartFrame(idx);
              AnimEndEdit.Value := mdl.EndFrame(idx);
            end;
          finally
            mdl.Free;
          end;
        end
{$IFNDEF NO_MODELX}
        else if UpperCase(ExtractFileExt(ModelEdit.Text)) = UpperCase(rsExtX) then
        begin
          mdl := TModelX.Create(ModelEdit.Text, f);
          try
            idx := mdl.frameNames.IndexOf(oldMove);
            if idx > -1 then
            begin
              AnimStartEdit.Value := mdl.StartFrame(idx);
              AnimEndEdit.Value := mdl.EndFrame(idx);
            end;
          finally
            mdl.Free;
          end;
        end{$ENDIF};
      finally
        f.Free;
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TOpenMD2Form.TextureEditBtn1Click(Sender: TObject);
begin
  if not FileExists(OpenDialog1.FileName) then OpenDialog1.FileName := '';
  if OpenDialog1.Execute then
  begin
    TextureEdit.Text := OpenDialog1.FileName;
    Info.TextureNames[0, 0] := OpenDialog1.FileName;
  end;
end;

end.
