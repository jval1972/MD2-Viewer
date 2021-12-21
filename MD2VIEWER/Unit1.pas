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
//  Main Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Old Site: http://www.geocities.ws/jimmyvalavanis/applications/md2viewer.html
//  New Site: https://sourceforge.net/projects/md2-viewer/
//------------------------------------------------------------------------------

{$I defs.inc}

unit Unit1;

{
  Version 1.3 (2018)
  Project ressurection
-------------------------
  Version 1.2 (2004????)
 1. Wireframe option
 2. Fog option
-------------------------
  Version 1.1 (20040608)
 1. Virtual Floor.
 2. Select animation speed
 3. Reset view button
-------------------------
  Version 1.0 (20040529)
  Initial release
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ToolWin, ComCtrls, Menus, ExtDlgs, AppEvnts, Tabs, ExtCtrls,
  StdCtrls,
  se_Main, se_DirectX, se_DXClass, se_DXDraws, se_D3DUtils, se_Quake2Utils,
  se_MyD3DUtils, se_DXInput, se_RTLCompileParams,
  MessageBox, XPMenu, Aboutdlg, AnotherReg,ImgList, ShellAPI, xM8, xPPM, xTGA,
  zBitmap, jpeg, xGIF, pcximage, dibimage, pngimage;

type
  TModelViewerScene = class(TD3DScene)
  public
    procedure DrawLoop; override;
  end;


  TDXViewerForm = class(TDXForm)
    ApplicationEvents1: TApplicationEvents;
    ToolBar1: TToolBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ToolButton1: TToolButton;
    Open2: TSpeedButton;
    ToolButton2: TToolButton;
    DisplayModeBox: TComboBox;
    FullScreen2: TSpeedButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    PrevMap: TSpeedButton;
    ComboBox1: TComboBox;
    NextMap: TSpeedButton;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Copy2: TSpeedButton;
    ToolButton5: TToolButton;
    View1: TMenuItem;
    FullScreen1: TMenuItem;
    StatusBar1: TStatusBar;
    ImageList1: TImageList;
    Contactme1: TMenuItem;
    N2: TMenuItem;
    QuickInfo1: TMenuItem;
    Homepage1: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    Save2: TSpeedButton;
    SaveAs1: TMenuItem;
    ToolButton6: TToolButton;
    ResetPositionButton: TSpeedButton;
    ViewFloor1: TMenuItem;
    Options1: TMenuItem;
    Fog1: TMenuItem;
    Wireframe1: TMenuItem;
    TextureFiltering1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ApplicationEvents1Activate(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FullScreenClick(Sender: TObject);
    procedure DisplayModeBoxChange(Sender: TObject);
    procedure PrevMapClick(Sender: TObject);
    procedure NextMapClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure ApplicationEvents1Hint(Sender: TObject);
    procedure Contactme1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ApplicationEvents1Deactivate(Sender: TObject);
    procedure QuickInfo1Click(Sender: TObject);
    procedure Homepage1Click(Sender: TObject);
    procedure Save2Click(Sender: TObject);
    procedure ResetPositionButtonClick(Sender: TObject);
    procedure ViewFloor1Click(Sender: TObject);
    procedure Fog1Click(Sender: TObject);
    procedure Wireframe1Click(Sender: TObject);
    procedure TextureFiltering1Click(Sender: TObject);
  private
    { Private declarations }
    DXDraw: TDXDraw;
    DXTimer: TDXTimer;
    DXInput: TDXInput;

    mmXPMenu1: TmmXPMenu;
    AboutDialog1: TAboutDialog;

    regFormRestorer1: TFormRestorer;
    regSceneBitCount: TVariantProfile;
    regSceneWidth: TVariantProfile;
    regSceneHeight: TVariantProfile;
    regViewFloor: TVariantProfile;
    regShowFog: TVariantProfile;
    regWireFrame: TVariantProfile;
    regTextureFiltering: TVariantProfile;

    ErrorNoMapSelectedMessageBox: TMessageBox;
    ErrorNoFileSelectMessageBox: TMessageBox;
    ClipboardErrorMessageBox: TMessageBox;
    ErrorMessageBox: TMessageBox;
    MessageBox1: TMessageBox;

    Scene: TModelViewerScene;
    WalkTime: double;
    OldTime: double; // Χρονική στιγμή του τελευταίου D3D Render

    procedure ResetPosition;
    procedure FrameMovie(Time: Double);
    procedure DrawTheScene;

    procedure LoadMap;

    procedure AdjustFocus;
    procedure AdjustFullScreen(const rlevel: integer = 0);
    procedure AdjustFog;

    procedure DXTimerTimer(Sender: TObject; LagCount: Integer);
    procedure DXDrawInitializeSurface(Sender: TObject);
    procedure DXDrawInitialize(Sender: TObject);
    procedure DXDrawFinalize(Sender: TObject);
    procedure DXDrawClick(Sender: TObject);
  protected

    IsLoading: boolean;
  // Sky support
    skyTexture: string;
    Texture_sky2: TDirect3DTexture2;
    SPHERE_0004_ID_0008_Vertexes: PD3DLVertexArray;

    FloorInfo: TD3DQuadrangleInfo;
    Floor: TD3DObject;

    serrormessage: string;

    doFiltering: boolean;

    procedure CreateComponents;
    procedure DestroyComponents;

    procedure UpdateModelFrameNames;
    procedure WMSysCommand(var Msg: TMessage); message WM_SysCommand;
  public
    { Public declarations }
    AppConfigKey1: TAppConfigKey;
    Info: TD3DActorInfo;
  end;

// Compiler $DEFINES
{
    NO_SCRIPTS,
    NO_D3DTRIANGLES,
    NO_D3DSTUBOBJECTS,
    NO_D3DEXOBJECTS,
    NO_D3DBILLBOARDS,
    NO_D3DCUBES,
    NO_D3DSPHERES,
    NO_D3DCONES,
    NO_D3DCYLINDERS,
    NO_D3DPLUGINS,
    NO_D3DRINGS,
    NO_D3DSECTORCOLLECTIONS,
    NO_D3DTEXTS,
    NO_D3DPROCEDURALOBJECTS,
    NO_D3DSOUNDS,
    NO_AVI.

NO_D3DTRIANGLES;NO_D3DSTUBOBJECTS;NO_D3DBILLBOARDS;NO_D3DCUBES;NO_D3DSPHERES;
NO_D3DCONES;NO_D3DCYLINDERS;NO_D3DRINGS;NO_D3DPLUGINS;NO_D3DTEXTS;
NO_D3DPROCEDURALOBJECTS;NO_SCRIPTS;NO_AVI;NO_D3DSOUNDS;NO_DOOMTHINGS;
NO_D3DEXOBJECTS;NO_D3DSECTORCOLLECTIONS}

var
  DXViewerForm: TDXViewerForm;

implementation

{$R *.DFM}

uses
  SyncObjs, Math, Clipbrd, Variants, se_DXDUtils, se_DXModels, se_Utils,
  md2v_globals, QuickInfoFrm, OpenMD2Frm;

type
  TD3DSceneDisplayParam = class(TObject)
    Width: integer;
    Height: integer;
    BitCount: integer;
    constructor Create(aWidth, aHeight, aBitCount: integer); virtual;
  end;

{ *** TD3DSceneDisplayParam *** }
constructor TD3DSceneDisplayParam.Create(aWidth, aHeight, aBitCount: integer);
begin
  Inherited Create;
  Width := aWidth;
  Height := aHeight;
  BitCount := aBitCount;
end;

procedure TModelViewerScene.DrawLoop;
var
  i: integer;
begin
  for i := 0 to Surfaces.Count - 1 do
  begin
    if (Surfaces.Objects[i] as TD3DObject).GetTypeID = ID3D_ACTOR then
    begin
      if DXViewerForm.Wireframe1.Checked then
      begin
         DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_FILLMODE, ord(D3DFILL_WIREFRAME));
        (Surfaces.Objects[i] as TD3DObject).DoDraw;
         DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_FILLMODE, ord(D3DFILL_SOLID));
      end
      else
        (Surfaces.Objects[i] as TD3DObject).DoDraw;
    end
    else
      (Surfaces.Objects[i] as TD3DObject).DoDraw;
  end;

  if (DXViewerForm.Texture_sky2 <> nil) and (DXViewerForm.skyTexture <> '') then
  begin
    DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_CULLMODE, Ord(D3DCULL_NONE));
    DXDraw.D3DDevice7.SetTexture(0, DXViewerForm.Texture_sky2.Surface.IDDSurface7);
    DXDraw.D3DDevice7.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_LVERTEX, DXViewerForm.SPHERE_0004_ID_0008_Vertexes[0], 320, 0);
  end;
end;

// Ορίζουμε τα keyboard assignes για το DXInput
function KeyAssignProc: TKeyAssignList;
begin
  FillChar(Result, SizeOf(Result), 0);

  AssignKey(Result, isUp,      [VK_UP, VK_NUMPAD8]);
  AssignKey(Result, isDown,    [VK_DOWN, VK_NUMPAD2]);
  AssignKey(Result, isLeft,    [VK_LEFT, VK_NUMPAD4]);
  AssignKey(Result, isRight,   [VK_RIGHT, VK_NUMPAD6]);
  AssignKey(Result, isButton1, [VK_NUMPAD9]);
  AssignKey(Result, isButton2, [VK_NUMPAD3]);
  AssignKey(Result, isButton3, [VK_ADD]);
  AssignKey(Result, isButton4, [VK_SUBTRACT]);

end;

procedure TDXViewerForm.WMSysCommand(var Msg: TMessage);
begin
  if (Msg.WParam = SC_SCREENSAVE) then
    if doFullScreen in DXDraw.NowOptions then
    begin
      Msg.Result := 0;
      exit;
    end;
  DefWindowProc(Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TDXViewerForm.DXTimerTimer(Sender: TObject; LagCount: Integer);
var
  cs: TCriticalSection;
begin
  cs := TCriticalSection.Create;
  try
    cs.Enter;
    DrawTheScene;
  finally
    cs.Release;
    cs.Free;
  end;
  if serrormessage <> '' then
  begin
    ErrorMessageBox.Execute(serrormessage);
    serrormessage := '';
  end;
end;

procedure TDXViewerForm.DrawTheScene;
begin
  if Scene <> nil then
  begin
    if DXDraw.CanDraw and not IsLoading then
    begin
      { Frame Movie }
      FrameMovie(Scene.Time);

      if doFiltering then
      begin
        DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MINFILTER, ord(D3DTFN_LINEAR));
        DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MAGFILTER, ord(D3DTFG_LINEAR));
      end
      else
      begin
        DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MINFILTER, ord(D3DTFN_POINT));
        DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MAGFILTER, ord(D3DTFG_POINT));
      end;

      Scene.DrawWithBackColor(clBlack);

      DXDraw.Flip;
    end;
  end;
end;

procedure TDXViewerForm.DXDrawInitializeSurface(Sender: TObject);
var
  vp: TD3DViewport7;
  mtrl: TD3DMaterial7;
  matProj: TD3DMatrix;
begin
  { Viewport }
  FillChar(vp, SizeOf(vp), 0);
  vp.dwX := 0;
  vp.dwY := 0;
  vp.dwWidth := DXDraw.SurfaceWidth;
  vp.dwHeight := DXDraw.SurfaceHeight;
  vp.dvMinZ := 0.0;
  vp.dvMaxZ := 1.0;

  DXDraw.D3DDevice7.SetViewport(vp);

  {  Material  }
  FillChar(mtrl, SizeOf(mtrl), 0);
  mtrl.ambient.r := 1.0;
  mtrl.ambient.g := 1.0;
  mtrl.ambient.b := 1.0;
  mtrl.ambient.a := 1.0;
  mtrl.specular.r := 0.0;
  mtrl.specular.g := 0.0;
  mtrl.specular.b := 0.0;
  mtrl.specular.a := 0.0;
  mtrl.diffuse.r := 1.0;
  mtrl.diffuse.g := 1.0;
  mtrl.diffuse.b := 1.0;
  DXDraw.D3DDevice7.SetMaterial(mtrl);
  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_AMBIENT, $ffffffff);
  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_SPECULARENABLE, 1);

// Set the projection matrix.
  FillChar(matProj, SizeOf(matProj), 0);
  matProj._11 :=  1.0;
  matProj._22 :=  1.0;
  matProj._33 :=  1.0;
  matProj._34 :=  1.0;
  matProj._43 := -1.0;
  DXDraw.D3DDevice7.SetTransform( D3DTRANSFORMSTATE_PROJECTION, matProj );

//Note: in DX7, setting D3DRENDERSTATE_LIGHTING to FALSE is needed to
// turn off vertex lighting (and use the color in the D3DLVERTEX instead.)
//  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_LIGHTING, 0);

  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_COLORARG1, ord(D3DTA_TEXTURE));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_COLORARG2, ord(D3DTA_DIFFUSE));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_COLOROP, ord(D3DTOP_MODULATE));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_ALPHAOP, ord(D3DTOP_SELECTARG1));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_ALPHAARG1, ord(D3DTA_TEXTURE));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MINFILTER, ord(D3DTFN_LINEAR));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MAGFILTER, ord(D3DTFG_LINEAR));
  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_NORMALIZENORMALS, ord(True));
  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_COLORKEYENABLE, 1);

//  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_ANTIALIAS, ord(D3DANTIALIAS_SORTINDEPENDENT));

  AdjustFog;

  if Scene <> nil then
    Scene.ForceRecalc;
end;

resourceString
  rsFmtMode = '%dx%d %dbit';
  rsFmtTitle = 'MD2 Model Viewer - %s';

const
  DefSceneWidth = 640;
  DefSceneHeight = 480;
  DefSceneBitCount = 16;
  ValidSceneBitCounts = [16, 32];

procedure TDXViewerForm.DXDrawInitialize(Sender: TObject);
var
  matView: TD3DMatrix;
  i: integer;
  aParam: TD3DSceneDisplayParam;
begin
  if DisplayModeBox.Items.Count = 0 then
  begin
    for i := 0 to DXDraw.Display.Count - 1 do
      if DXDraw.Display[i].BitCount in ValidSceneBitCounts then
      begin
        aParam := TD3DSceneDisplayParam.Create(
          DXDraw.Display[i].Width,
          DXDraw.Display[i].Height,
          DXDraw.Display[i].BitCount);
        DisplayModeBox.Items.AddObject(Format(rsFmtMode,
          [aParam.Width, aParam.Height, aParam.BitCount]), aParam);
      end;
    if VarIsEmpty(regSceneWidth.Value) then
      regSceneWidth.Value := DefSceneWidth;
    if VarIsEmpty(regSceneHeight.Value) then
      regSceneHeight.Value := DefSceneHeight;
    if VarIsEmpty(regSceneBitCount.Value) then
      regSceneBitCount.Value := DefSceneBitCount;
    DisplayModeBox.ItemIndex := DisplayModeBox.Items.IndexOf(
      Format(rsFmtMode,
        [Integer(regSceneWidth.Value),
         Integer(regSceneHeight.Value),
         Integer(regSceneBitCount.Value)]));
    if DisplayModeBox.ItemIndex = - 1 then
      DisplayModeBox.ItemIndex := DisplayModeBox.Items.IndexOf(
        Format(rsFmtMode,[DefSceneWidth, DefSceneHeight, DefSceneBitCount]));
    if DXDraw.CanFocus then
      DXDraw.SetFocus;
  end;

  FillChar(matView, SizeOf(matView), 0);
  matView._11 := 0.75; // Screen Aspect ratio
  matView._22 := 1;
  matView._33 := 1;
  DXDraw.D3DDevice7.SetTransform( D3DTRANSFORMSTATE_VIEW, matView);
end;

procedure TDXViewerForm.CreateComponents;
begin
  DXDraw := TDXDraw.Create(self);
  DXDraw.Parent := self;
  DXDraw.Left := 0;
  DXDraw.Top := 0;
  DXDraw.Width := 640;
  DXDraw.Height := 480;
  DXDraw.AutoInitialize := True;
  DXDraw.AutoSize := True;
  DXDraw.Color := clBlack;
  DXDraw.Display.FixedBitCount := False;
  DXDraw.Display.FixedRatio := True;
  DXDraw.Display.FixedSize := False;
  DXDraw.Options := [doAllowReboot, doWaitVBlank, doAllowPalette256, doStretch, doCenter, doFlip, do3D, doDirectX7Mode, doHardware, doSelectDriver, doZBuffer];
  DXDraw.SurfaceHeight := 480;
  DXDraw.OnFinalize := DXDrawFinalize;
  DXDraw.OnInitialize := DXDrawInitialize;
  DXDraw.OnInitializeSurface := DXDrawInitializeSurface;
  DXDraw.Align := alClient;
  DXDraw.DragMode := dmAutomatic;
  DXDraw.TabOrder := 2;
  DXDraw.Visible := False;
  DXDraw.OnClick := DXDrawClick;

  DXTimer := TDXTimer.Create(self);
  DXTimer.ActiveOnly := True;
  DXTimer.Enabled := False;
  DXTimer.Interval := 1;
  DXTimer.OnTimer := DXTimerTimer;

  DXInput := TDXInput.Create(self);
  DXInput.UseDirectInput := False;
  DXInput.UseDirectInput := True;
  DXInput.ActiveOnly := True;

  DXInput.Joystick.BindInputStates := True;
  DXInput.Joystick.Enabled := True;
  DXInput.Joystick.ForceFeedback := False;
  DXInput.Joystick.AutoCenter := True;
  DXInput.Joystick.DeadZoneX := 50;
  DXInput.Joystick.DeadZoneY := 50;
  DXInput.Joystick.DeadZoneZ := 50;
  DXInput.Joystick.ID := 0;
  DXInput.Joystick.RangeX := 1000;
  DXInput.Joystick.RangeY := 1000;
  DXInput.Joystick.RangeZ := 1000;

  DXInput.Keyboard.BindInputStates := True;
  DXInput.Keyboard.Enabled := True;
  DXInput.Keyboard.ForceFeedback := False;

  DXInput.Mouse.BindInputStates := True;
  DXInput.Mouse.Enabled := True;
  DXInput.Mouse.ForceFeedback := False;

  mmXPMenu1 := TmmXPMenu.Create(self);
  mmXPMenu1.Font.Charset := DEFAULT_CHARSET;
  mmXPMenu1.Font.Color := clMenuText;
  mmXPMenu1.Font.Height := -11;
  mmXPMenu1.Font.Name := 'Tahoma';
  mmXPMenu1.Font.Style := [];
  mmXPMenu1.Color := clBtnFace;
  mmXPMenu1.IconBackColor := clBtnFace;
  mmXPMenu1.MenuBarColor := clBtnFace;
  mmXPMenu1.SelectColor := clHighlight;
  mmXPMenu1.SelectBorderColor := clHighlight;
  mmXPMenu1.SelectFontColor := clMenuText;
  mmXPMenu1.DisabledColor := clInactiveCaption;
  mmXPMenu1.SeparatorColor := clBtnFace;
  mmXPMenu1.CheckedColor := clHighlight;
  mmXPMenu1.IconWidth := 24;
  mmXPMenu1.DrawSelect := True;
  mmXPMenu1.UseSystemColors := True;
  mmXPMenu1.OverrideOwnerDraw := False;
  mmXPMenu1.Gradient := False;
  mmXPMenu1.FlatMenu := False;
  mmXPMenu1.MakeToolbars := False;
  mmXPMenu1.MakeControlBars := False;
  mmXPMenu1.AutoDetect := False;
  mmXPMenu1.Active := True;

  AboutDialog1 := TAboutDialog.Create(self);
  AboutDialog1.ProductName := 'MD2Viewer';
  AboutDialog1.Version := 'Version 1.3';
  AboutDialog1.Copyright := '© 2004-2018, Jim Valavanis, <jimmyvalavanis@yahoo.gr>';
  AboutDialog1.Comments := 'MD2 Model Viewer for Windows';

  AppConfigKey1 := TAppConfigKey.Create(self);
  AppConfigKey1.ApplicationName := 'MD2Viewer';
  AppConfigKey1.ApplicationVersion := '1.3';
  AppConfigKey1.CompanyName := 'Jim Valavanis';
  AppConfigKey1.Name := 'AppConfigKey1';

  regFormRestorer1 := TFormRestorer.Create(self);
  regFormRestorer1.ParentKey := AppConfigKey1;
  regFormRestorer1.Name := 'FormRestorer1';
  regFormRestorer1.Restoring := frSizeAndPosition;
  regFormRestorer1.Restore;

  regSceneBitCount := TVariantProfile.Create(self);
  regSceneBitCount.Key := regFormRestorer1;
  regSceneBitCount.Name := 'SceneBitCount';

{  regShowInformation := TVariantProfile.Create(self);
  regShowInformation.Key := regFormRestorer1;
  regShowInformation.Name := 'ShowInformation';}

  regSceneWidth := TVariantProfile.Create(self);
  regSceneWidth.Key := regFormRestorer1;
  regSceneWidth.Name := 'SceneWidth';

  regSceneHeight := TVariantProfile.Create(self);
  regSceneHeight.Key := regFormRestorer1;
  regSceneHeight.Name := 'SceneHeight';

{  regUseHardwareAcceleration := TVariantProfile.Create(self);
  regUseHardwareAcceleration.Key := regFormRestorer1;
  regUseHardwareAcceleration.Name := 'UseHardwareAcceleration';}

{  regSafeMode := TVariantProfile.Create(self);
  regSafeMode.Key := regFormRestorer1;
  regSafeMode.Name := 'SafeMode';}

{  regTextureFiltering := TVariantProfile.Create(self);
  regTextureFiltering.Key := regFormRestorer1;
  regTextureFiltering.Name := 'TextureFiltering';}

{  regTransparent := TVariantProfile.Create(self);
  regTransparent.Key := regFormRestorer1;
  regTransparent.Name := 'Transparent';}

{  regLightFactor := TVariantProfile.Create(self);
  regLightFactor.Key := regFormRestorer1;
  regLightFactor.Name := 'LightFactor';}

  regShowFog := TVariantProfile.Create(self);
  regShowFog.Key := regFormRestorer1;
  regShowFog.Name := 'ShowFog';

  regViewFloor := TVariantProfile.Create(self);
  regViewFloor.Key := regFormRestorer1;
  regViewFloor.Name := 'ViewFloor';

  regWireFrame := TVariantProfile.Create(self);
  regWireFrame.Key := regFormRestorer1;
  regWireFrame.Name := 'WireFrame';

  regTextureFiltering := TVariantProfile.Create(self);
  regTextureFiltering.Key := regFormRestorer1;
  regTextureFiltering.Name := 'TextureFiltering';

  ErrorNoMapSelectedMessageBox := TMessageBox.Create(self);
  ErrorNoMapSelectedMessageBox.Caption := 'MD2 Model Viewer';
  ErrorNoMapSelectedMessageBox.Text := 'You did not select a valid filename.';
  ErrorNoMapSelectedMessageBox.Buttons := mbxOK;
  ErrorNoMapSelectedMessageBox.Icon := mbxIconError;
  ErrorNoMapSelectedMessageBox.DefaultButton := mbxDefButton1;
  ErrorNoMapSelectedMessageBox.Modality := mbxTaskModal;
  ErrorNoMapSelectedMessageBox.TextAlignment := mbxLeft;

  ErrorNoFileSelectMessageBox := TMessageBox.Create(self);
  ErrorNoFileSelectMessageBox.Caption := 'MD2 Model Viewer';
  ErrorNoFileSelectMessageBox.Text := 'You did not select a valid filename.';
  ErrorNoFileSelectMessageBox.Buttons := mbxOK;
  ErrorNoFileSelectMessageBox.Icon := mbxIconError;
  ErrorNoFileSelectMessageBox.DefaultButton := mbxDefButton1;
  ErrorNoFileSelectMessageBox.Modality := mbxTaskModal;
  ErrorNoFileSelectMessageBox.TextAlignment := mbxLeft;

  ClipboardErrorMessageBox := TMessageBox.Create(self);
  ClipboardErrorMessageBox.Caption := 'MD2 Model Viewer';
  ClipboardErrorMessageBox.Text := 'Can not copy to clipboard an empty bitmap!';
  ClipboardErrorMessageBox.Buttons := mbxOK;
  ClipboardErrorMessageBox.Icon := mbxIconError;
  ClipboardErrorMessageBox.DefaultButton := mbxDefButton1;
  ClipboardErrorMessageBox.Modality := mbxTaskModal;
  ClipboardErrorMessageBox.TextAlignment := mbxLeft;

  ErrorMessageBox := TMessageBox.Create(self);
  ErrorMessageBox.Caption := 'MD2 Model Viewer';
  ErrorMessageBox.Text := 'Write your message here.';
  ErrorMessageBox.Buttons := mbxOK;
  ErrorMessageBox.Icon := mbxIconStop;
  ErrorMessageBox.DefaultButton := mbxDefButton1;
  ErrorMessageBox.Modality := mbxTaskModal;
  ErrorMessageBox.TextAlignment := mbxLeft;

  MessageBox1 := TMessageBox.Create(self);
  MessageBox1.Caption := 'MD2 Model Viewer';
  MessageBox1.Text := 'Can not determine file type!';
  MessageBox1.Buttons := mbxOK;
  MessageBox1.Icon := mbxIconStop;
  MessageBox1.DefaultButton := mbxDefButton1;
  MessageBox1.Modality := mbxDefModality;
  MessageBox1.TextAlignment := mbxLeft;
end;

procedure TDXViewerForm.DestroyComponents;
begin
  regShowFog.Free;
  regSceneBitCount.Free;
  regSceneWidth.Free;
  regSceneHeight.Free;
  regWireFrame.Free;
  regTextureFiltering.Free;
  regFormRestorer1.Store;
  regFormRestorer1.Free;
  AppConfigKey1.Free;

  mmXPMenu1.Free;
  AboutDialog1.Free;

  ErrorNoMapSelectedMessageBox.Free;
  ErrorNoFileSelectMessageBox.Free;
  ClipboardErrorMessageBox.Free;
  MessageBox1.Free;
  ErrorMessageBox.Free;
end;

procedure TDXViewerForm.FormCreate(Sender: TObject);
begin
  Visible := False;
  Screen.Cursor := crHourglass;

  try
    CreateComponents;

    Color := clBlack;

    serrormessage := '';

    IsLoading := False;
    skyTexture := '';
    Texture_sky2 := nil;

    mmXPMenu1.Active := (lobyte(loword(GetVersion)) = 5) and
                        (hibyte(loword(GetVersion)) = 0);

    FillChar(Info, SizeOf(Info), Chr(0));
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
    Info.UseInterpolation := True;
    Info.AnimStart := 0;
    Info.AnimEnd := 9;
    Info.AnimSpeed := 2.0;

    if VarIsEmpty(regViewFloor.Value) then
      ViewFloor1.Checked := True
    else
      ViewFloor1.Checked := regViewFloor.Value = 1;

    if VarIsEmpty(regWireFrame.Value) then
      WireFrame1.Checked := False
    else
      WireFrame1.Checked := regWireFrame.Value = 1;

    if not VarIsEmpty(regTextureFiltering.Value) then
      doFiltering := regTextureFiltering.Value
    else
      doFiltering := False;
    TextureFiltering1.Checked := doFiltering;

    if VarIsEmpty(regShowFog.Value) then
      Fog1.Checked := True
    else
      Fog1.Checked := regShowFog.Value = 1;

    Floor := nil;
    FillChar(FloorInfo, SizeOf(FloorInfo), Chr(0));
    FloorInfo.Key := 1;
    FloorInfo.x1 := -10000.0;
    FloorInfo.z1 :=  10000.0;
    FloorInfo.x2 :=  10000.0;
    FloorInfo.z2 :=  10000.0;
    FloorInfo.x3 := -10000.0;
    FloorInfo.z3 := -10000.0;
    FloorInfo.x4 :=  10000.0;
    FloorInfo.z4 := -10000.0;
    FloorInfo.C1 := RGB(192, 192, 192);
    FloorInfo.C2 := RGB(192, 192, 192);
    FloorInfo.C3 := RGB(192, 192, 192);
    FloorInfo.C4 := RGB(192, 192, 192);
    FloorInfo.flags := flg_DoubleSided;
    FloorInfo.u := 0.0;
    FloorInfo.v := 0.0;
    FloorInfo.Transparent := False;
    FloorInfo.TextureName := '';

    DXDraw.Align := alClient;

    Scene := TModelViewerScene.Create(DXDraw);
    Scene.NearClippingPlane := 0.5;

    DXDraw.Visible := True;
    DXTimer.Enabled := True;
    WalkTime := 0.0;
    oldTime := GetTickCount / 1000;
    DXInput.Keyboard.Enabled := True;
    DXInput.Keyboard.BindInputStates := True;
    DXInput.Mouse.Enabled := False;
    DXInput.Mouse.BindInputStates := False;
    DXInput.Joystick.Enabled := True;
    DXInput.Joystick.BindInputStates := True;
    DXInput.Keyboard.KeyAssigns := KeyAssignProc;

{    if not VarIsEmpty(regUseHardwareAcceleration.Value) then
      doUseHardware := regUseHardwareAcceleration.Value
    else
      doUseHardware := True;
    SetHardware(doUseHardware, True);}

{    if not VarIsEmpty(regTextureFiltering.Value) then
      doFiltering := regTextureFiltering.Value
    else
      doFiltering := True;}


////////////////////////////////////////////////////////////////////////////////
// Sky support
    GetMem(SPHERE_0004_ID_0008_Vertexes, 320 * SizeOf(TD3DLVertex));
    SPHERE_0004_ID_0008_Vertexes[0] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[1] := MakeD3DLVERTEX(0.000000000000, 6755.282714843750, 3090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[2] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[3] := MakeD3DLVERTEX(1256.885375976563, 6755.282714843750, 2823.010742187500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[4] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[5] := MakeD3DLVERTEX(2296.444091796875, 6755.282714843750, 2067.727050781250, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[6] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[7] := MakeD3DLVERTEX(2938.926513671875, 6755.282714843750, 954.914978027344, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[8] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[9] := MakeD3DLVERTEX(3073.241699218750, 6755.282714843750, -323.010864257813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[10] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[11] := MakeD3DLVERTEX(2676.165771484375, 6755.282714843750, -1545.085205078125, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[12] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[13] := MakeD3DLVERTEX(1816.356201171875, 6755.282714843750, -2500.000244140625, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[14] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[15] := MakeD3DLVERTEX(642.482177734375, 6755.282714843750, -3022.642333984375, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[16] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[17] := MakeD3DLVERTEX(-642.482788085938, 6755.282714843750, -3022.642333984375, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[18] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[19] := MakeD3DLVERTEX(-1816.356811523438, 6755.282714843750, -2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[20] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[21] := MakeD3DLVERTEX(-2676.166015625000, 6755.282714843750, -1545.084716796875, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[22] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[23] := MakeD3DLVERTEX(-3073.241699218750, 6755.282714843750, -323.010345458984, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[24] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[25] := MakeD3DLVERTEX(-2938.926269531250, 6755.282714843750, 954.915405273438, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[26] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[27] := MakeD3DLVERTEX(-2296.443603515625, 6755.282714843750, 2067.727783203125, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[28] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[29] := MakeD3DLVERTEX(-1256.884887695313, 6755.282714843750, 2823.010986328125, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[30] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[31] := MakeD3DLVERTEX(0.000540302484, 6755.282714843750, 3090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[32] := MakeD3DLVERTEX(0.000000000000, 6755.282714843750, 3090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[33] := MakeD3DLVERTEX(0.000000000000, 6045.084960937500, 5877.852539062500, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[34] := MakeD3DLVERTEX(1256.885375976563, 6755.282714843750, 2823.010742187500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[35] := MakeD3DLVERTEX(2390.738037109375, 6045.084960937500, 5369.685058593750, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[36] := MakeD3DLVERTEX(2296.444091796875, 6755.282714843750, 2067.727050781250, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[37] := MakeD3DLVERTEX(4368.096191406250, 6045.084960937500, 3933.050537109375, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[38] := MakeD3DLVERTEX(2938.926513671875, 6755.282714843750, 954.914978027344, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[39] := MakeD3DLVERTEX(5590.169921875000, 6045.084960937500, 1816.356201171875, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[40] := MakeD3DLVERTEX(3073.241699218750, 6755.282714843750, -323.010864257813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[41] := MakeD3DLVERTEX(5845.652832031250, 6045.084960937500, -614.403137207031, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[42] := MakeD3DLVERTEX(2676.165771484375, 6755.282714843750, -1545.085205078125, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[43] := MakeD3DLVERTEX(5090.369628906250, 6045.084960937500, -2938.926513671875, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[44] := MakeD3DLVERTEX(1816.356201171875, 6755.282714843750, -2500.000244140625, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[45] := MakeD3DLVERTEX(3454.914794921875, 6045.084960937500, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[46] := MakeD3DLVERTEX(642.482177734375, 6755.282714843750, -3022.642333984375, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[47] := MakeD3DLVERTEX(1222.073730468750, 6045.084960937500, -5749.407226562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[48] := MakeD3DLVERTEX(-642.482788085938, 6755.282714843750, -3022.642333984375, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[49] := MakeD3DLVERTEX(-1222.074829101563, 6045.084960937500, -5749.406738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[50] := MakeD3DLVERTEX(-1816.356811523438, 6755.282714843750, -2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[51] := MakeD3DLVERTEX(-3454.915527343750, 6045.084960937500, -4755.282226562500, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[52] := MakeD3DLVERTEX(-2676.166015625000, 6755.282714843750, -1545.084716796875, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[53] := MakeD3DLVERTEX(-5090.369628906250, 6045.084960937500, -2938.925537109375, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[54] := MakeD3DLVERTEX(-3073.241699218750, 6755.282714843750, -323.010345458984, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[55] := MakeD3DLVERTEX(-5845.652832031250, 6045.084960937500, -614.402160644531, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[56] := MakeD3DLVERTEX(-2938.926269531250, 6755.282714843750, 954.915405273438, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[57] := MakeD3DLVERTEX(-5590.169433593750, 6045.084960937500, 1816.357055664063, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[58] := MakeD3DLVERTEX(-2296.443603515625, 6755.282714843750, 2067.727783203125, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[59] := MakeD3DLVERTEX(-4368.095214843750, 6045.084960937500, 3933.051757812500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[60] := MakeD3DLVERTEX(-1256.884887695313, 6755.282714843750, 2823.010986328125, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[61] := MakeD3DLVERTEX(-2390.737060546875, 6045.084960937500, 5369.686035156250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[62] := MakeD3DLVERTEX(0.000540302484, 6755.282714843750, 3090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[63] := MakeD3DLVERTEX(0.001027716324, 6045.084960937500, 5877.852539062500, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[64] := MakeD3DLVERTEX(0.000000000000, 6045.084960937500, 5877.852539062500, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[65] := MakeD3DLVERTEX(0.000000000000, 4938.926269531250, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[66] := MakeD3DLVERTEX(2390.738037109375, 6045.084960937500, 5369.685058593750, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[67] := MakeD3DLVERTEX(3290.568603515625, 4938.926269531250, 7390.738281250000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[68] := MakeD3DLVERTEX(4368.096191406250, 6045.084960937500, 3933.050537109375, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[69] := MakeD3DLVERTEX(6012.168457031250, 4938.926269531250, 5413.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[70] := MakeD3DLVERTEX(5590.169921875000, 6045.084960937500, 1816.356201171875, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[71] := MakeD3DLVERTEX(7694.208984375000, 4938.926269531250, 2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[72] := MakeD3DLVERTEX(5845.652832031250, 6045.084960937500, -614.403137207031, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[73] := MakeD3DLVERTEX(8045.851562500000, 4938.926269531250, -845.653442382813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[74] := MakeD3DLVERTEX(5090.369628906250, 6045.084960937500, -2938.926513671875, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[75] := MakeD3DLVERTEX(7006.292480468750, 4938.926269531250, -4045.085693359375, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[76] := MakeD3DLVERTEX(3454.914794921875, 6045.084960937500, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[77] := MakeD3DLVERTEX(4755.282226562500, 4938.926269531250, -6545.085449218750, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[78] := MakeD3DLVERTEX(1222.073730468750, 6045.084960937500, -5749.407226562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[79] := MakeD3DLVERTEX(1682.040283203125, 4938.926269531250, -7913.380859375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[80] := MakeD3DLVERTEX(-1222.074829101563, 6045.084960937500, -5749.406738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[81] := MakeD3DLVERTEX(-1682.041748046875, 4938.926269531250, -7913.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[82] := MakeD3DLVERTEX(-3454.915527343750, 6045.084960937500, -4755.282226562500, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[83] := MakeD3DLVERTEX(-4755.283691406250, 4938.926269531250, -6545.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[84] := MakeD3DLVERTEX(-5090.369628906250, 6045.084960937500, -2938.925537109375, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[85] := MakeD3DLVERTEX(-7006.292968750000, 4938.926269531250, -4045.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[86] := MakeD3DLVERTEX(-5845.652832031250, 6045.084960937500, -614.402160644531, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[87] := MakeD3DLVERTEX(-8045.851562500000, 4938.926269531250, -845.651977539063, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[88] := MakeD3DLVERTEX(-5590.169433593750, 6045.084960937500, 1816.357055664063, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[89] := MakeD3DLVERTEX(-7694.208496093750, 4938.926269531250, 2500.001220703125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[90] := MakeD3DLVERTEX(-4368.095214843750, 6045.084960937500, 3933.051757812500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[91] := MakeD3DLVERTEX(-6012.167480468750, 4938.926269531250, 5413.381347656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[92] := MakeD3DLVERTEX(-2390.737060546875, 6045.084960937500, 5369.686035156250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[93] := MakeD3DLVERTEX(-3290.567382812500, 4938.926269531250, 7390.738769531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[94] := MakeD3DLVERTEX(0.001027716324, 6045.084960937500, 5877.852539062500, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[95] := MakeD3DLVERTEX(0.001414530328, 4938.926269531250, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[96] := MakeD3DLVERTEX(0.000000000000, 4938.926269531250, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[97] := MakeD3DLVERTEX(0.000000000000, 3545.084960937500, 9510.565429687500, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[98] := MakeD3DLVERTEX(3290.568603515625, 4938.926269531250, 7390.738281250000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[99] := MakeD3DLVERTEX(3868.295654296875, 3545.084960937500, 8688.333984375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[100] := MakeD3DLVERTEX(6012.168457031250, 4938.926269531250, 5413.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[101] := MakeD3DLVERTEX(7067.728027343750, 3545.084960937500, 6363.809570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[102] := MakeD3DLVERTEX(7694.208984375000, 4938.926269531250, 2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[103] := MakeD3DLVERTEX(9045.084960937500, 3545.084960937500, 2938.926269531250, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[104] := MakeD3DLVERTEX(8045.851562500000, 4938.926269531250, -845.653442382813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[105] := MakeD3DLVERTEX(9458.465820312500, 3545.084960937500, -994.125244140625, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[106] := MakeD3DLVERTEX(7006.292480468750, 4938.926269531250, -4045.085693359375, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[107] := MakeD3DLVERTEX(8236.390625000000, 3545.084960937500, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[108] := MakeD3DLVERTEX(4755.282226562500, 4938.926269531250, -6545.085449218750, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[109] := MakeD3DLVERTEX(5590.169433593750, 3545.084960937500, -7694.209960937500, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[110] := MakeD3DLVERTEX(1682.040283203125, 4938.926269531250, -7913.380859375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[111] := MakeD3DLVERTEX(1977.356933593750, 3545.084960937500, -9302.737304687500, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[112] := MakeD3DLVERTEX(-1682.041748046875, 4938.926269531250, -7913.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[113] := MakeD3DLVERTEX(-1977.358642578125, 3545.084960937500, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[114] := MakeD3DLVERTEX(-4755.283691406250, 4938.926269531250, -6545.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[115] := MakeD3DLVERTEX(-5590.171386718750, 3545.084960937500, -7694.208496093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[116] := MakeD3DLVERTEX(-7006.292968750000, 4938.926269531250, -4045.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[117] := MakeD3DLVERTEX(-8236.391601562500, 3545.084960937500, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[118] := MakeD3DLVERTEX(-8045.851562500000, 4938.926269531250, -845.651977539063, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[119] := MakeD3DLVERTEX(-9458.465820312500, 3545.084960937500, -994.123596191406, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[120] := MakeD3DLVERTEX(-7694.208496093750, 4938.926269531250, 2500.001220703125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[121] := MakeD3DLVERTEX(-9045.084960937500, 3545.084960937500, 2938.927490234375, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[122] := MakeD3DLVERTEX(-6012.167480468750, 4938.926269531250, 5413.381347656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[123] := MakeD3DLVERTEX(-7067.727050781250, 3545.084960937500, 6363.811523437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[124] := MakeD3DLVERTEX(-3290.567382812500, 4938.926269531250, 7390.738769531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[125] := MakeD3DLVERTEX(-3868.293945312500, 3545.084960937500, 8688.334960937500, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[126] := MakeD3DLVERTEX(0.001414530328, 4938.926269531250, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[127] := MakeD3DLVERTEX(0.001662880066, 3545.084960937500, 9510.565429687500, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[128] := MakeD3DLVERTEX(0.000000000000, 3545.084960937500, 9510.565429687500, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[129] := MakeD3DLVERTEX(0.000000000000, 1999.999755859375, 10000.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[130] := MakeD3DLVERTEX(3868.295654296875, 3545.084960937500, 8688.333984375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[131] := MakeD3DLVERTEX(4067.366455078125, 1999.999755859375, 9135.454101562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[132] := MakeD3DLVERTEX(7067.728027343750, 3545.084960937500, 6363.809570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[133] := MakeD3DLVERTEX(7431.448730468750, 1999.999755859375, 6691.305664062500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[134] := MakeD3DLVERTEX(9045.084960937500, 3545.084960937500, 2938.926269531250, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[135] := MakeD3DLVERTEX(9510.565429687500, 1999.999755859375, 3090.169677734375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[136] := MakeD3DLVERTEX(9458.465820312500, 3545.084960937500, -994.125244140625, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[137] := MakeD3DLVERTEX(9945.218750000000, 1999.999755859375, -1045.285034179688, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[138] := MakeD3DLVERTEX(8236.390625000000, 3545.084960937500, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[139] := MakeD3DLVERTEX(8660.253906250000, 1999.999755859375, -5000.000488281250, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[140] := MakeD3DLVERTEX(5590.169433593750, 3545.084960937500, -7694.209960937500, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[141] := MakeD3DLVERTEX(5877.852050781250, 1999.999755859375, -8090.170410156250, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[142] := MakeD3DLVERTEX(1977.356933593750, 3545.084960937500, -9302.737304687500, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[143] := MakeD3DLVERTEX(2079.116210937500, 1999.999755859375, -9781.476562500000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[144] := MakeD3DLVERTEX(-1977.358642578125, 3545.084960937500, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[145] := MakeD3DLVERTEX(-2079.117919921875, 1999.999755859375, -9781.475585937500, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[146] := MakeD3DLVERTEX(-5590.171386718750, 3545.084960937500, -7694.208496093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[147] := MakeD3DLVERTEX(-5877.853515625000, 1999.999755859375, -8090.169433593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[148] := MakeD3DLVERTEX(-8236.391601562500, 3545.084960937500, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[149] := MakeD3DLVERTEX(-8660.254882812500, 1999.999755859375, -4999.999023437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[150] := MakeD3DLVERTEX(-9458.465820312500, 3545.084960937500, -994.123596191406, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[151] := MakeD3DLVERTEX(-9945.218750000000, 1999.999755859375, -1045.283325195313, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[152] := MakeD3DLVERTEX(-9045.084960937500, 3545.084960937500, 2938.927490234375, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[153] := MakeD3DLVERTEX(-9510.564453125000, 1999.999755859375, 3090.171142578125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[154] := MakeD3DLVERTEX(-7067.727050781250, 3545.084960937500, 6363.811523437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[155] := MakeD3DLVERTEX(-7431.447265625000, 1999.999755859375, 6691.307617187500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[156] := MakeD3DLVERTEX(-3868.293945312500, 3545.084960937500, 8688.334960937500, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[157] := MakeD3DLVERTEX(-4067.364990234375, 1999.999755859375, 9135.455078125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[158] := MakeD3DLVERTEX(0.001662880066, 3545.084960937500, 9510.565429687500, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[159] := MakeD3DLVERTEX(0.001748455572, 1999.999755859375, 10000.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[160] := MakeD3DLVERTEX(0.000000000000, 1999.999755859375, 10000.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[161] := MakeD3DLVERTEX(0.000000000000, 454.914794921875, 9510.564453125000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[162] := MakeD3DLVERTEX(4067.366455078125, 1999.999755859375, 9135.454101562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[163] := MakeD3DLVERTEX(3868.295166015625, 454.914794921875, 8688.333007812500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[164] := MakeD3DLVERTEX(7431.448730468750, 1999.999755859375, 6691.305664062500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[165] := MakeD3DLVERTEX(7067.727539062500, 454.914794921875, 6363.809570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[166] := MakeD3DLVERTEX(9510.565429687500, 1999.999755859375, 3090.169677734375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[167] := MakeD3DLVERTEX(9045.084960937500, 454.914794921875, 2938.926025390625, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[168] := MakeD3DLVERTEX(9945.218750000000, 1999.999755859375, -1045.285034179688, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[169] := MakeD3DLVERTEX(9458.464843750000, 454.914794921875, -994.125183105469, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[170] := MakeD3DLVERTEX(8660.253906250000, 1999.999755859375, -5000.000488281250, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[171] := MakeD3DLVERTEX(8236.390625000000, 454.914794921875, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[172] := MakeD3DLVERTEX(5877.852050781250, 1999.999755859375, -8090.170410156250, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[173] := MakeD3DLVERTEX(5590.168945312500, 454.914794921875, -7694.208984375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[174] := MakeD3DLVERTEX(2079.116210937500, 1999.999755859375, -9781.476562500000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[175] := MakeD3DLVERTEX(1977.356811523438, 454.914794921875, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[176] := MakeD3DLVERTEX(-2079.117919921875, 1999.999755859375, -9781.475585937500, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[177] := MakeD3DLVERTEX(-1977.358642578125, 454.914794921875, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[178] := MakeD3DLVERTEX(-5877.853515625000, 1999.999755859375, -8090.169433593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[179] := MakeD3DLVERTEX(-5590.170410156250, 454.914794921875, -7694.208007812500, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[180] := MakeD3DLVERTEX(-8660.254882812500, 1999.999755859375, -4999.999023437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[181] := MakeD3DLVERTEX(-8236.390625000000, 454.914794921875, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[182] := MakeD3DLVERTEX(-9945.218750000000, 1999.999755859375, -1045.283325195313, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[183] := MakeD3DLVERTEX(-9458.464843750000, 454.914794921875, -994.123535156250, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[184] := MakeD3DLVERTEX(-9510.564453125000, 1999.999755859375, 3090.171142578125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[185] := MakeD3DLVERTEX(-9045.083984375000, 454.914794921875, 2938.927490234375, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[186] := MakeD3DLVERTEX(-7431.447265625000, 1999.999755859375, 6691.307617187500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[187] := MakeD3DLVERTEX(-7067.726074218750, 454.914794921875, 6363.811523437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[188] := MakeD3DLVERTEX(-4067.364990234375, 1999.999755859375, 9135.455078125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[189] := MakeD3DLVERTEX(-3868.293701171875, 454.914794921875, 8688.333984375000, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[190] := MakeD3DLVERTEX(0.001748455572, 1999.999755859375, 10000.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[191] := MakeD3DLVERTEX(0.001662879949, 454.914794921875, 9510.564453125000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[192] := MakeD3DLVERTEX(0.000000000000, 454.914794921875, 9510.564453125000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[193] := MakeD3DLVERTEX(0.000000000000, -938.926025390625, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[194] := MakeD3DLVERTEX(3868.295166015625, 454.914794921875, 8688.333007812500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[195] := MakeD3DLVERTEX(3290.568603515625, -938.926025390625, 7390.738281250000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[196] := MakeD3DLVERTEX(7067.727539062500, 454.914794921875, 6363.809570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[197] := MakeD3DLVERTEX(6012.168457031250, -938.926025390625, 5413.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[198] := MakeD3DLVERTEX(9045.084960937500, 454.914794921875, 2938.926025390625, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[199] := MakeD3DLVERTEX(7694.208984375000, -938.926025390625, 2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[200] := MakeD3DLVERTEX(9458.464843750000, 454.914794921875, -994.125183105469, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[201] := MakeD3DLVERTEX(8045.851562500000, -938.926025390625, -845.653442382813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[202] := MakeD3DLVERTEX(8236.390625000000, 454.914794921875, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[203] := MakeD3DLVERTEX(7006.292480468750, -938.926025390625, -4045.085693359375, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[204] := MakeD3DLVERTEX(5590.168945312500, 454.914794921875, -7694.208984375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[205] := MakeD3DLVERTEX(4755.282226562500, -938.926025390625, -6545.085449218750, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[206] := MakeD3DLVERTEX(1977.356811523438, 454.914794921875, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[207] := MakeD3DLVERTEX(1682.040283203125, -938.926025390625, -7913.380859375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[208] := MakeD3DLVERTEX(-1977.358642578125, 454.914794921875, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[209] := MakeD3DLVERTEX(-1682.041748046875, -938.926025390625, -7913.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[210] := MakeD3DLVERTEX(-5590.170410156250, 454.914794921875, -7694.208007812500, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[211] := MakeD3DLVERTEX(-4755.283691406250, -938.926025390625, -6545.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[212] := MakeD3DLVERTEX(-8236.390625000000, 454.914794921875, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[213] := MakeD3DLVERTEX(-7006.292968750000, -938.926025390625, -4045.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[214] := MakeD3DLVERTEX(-9458.464843750000, 454.914794921875, -994.123535156250, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[215] := MakeD3DLVERTEX(-8045.851562500000, -938.926025390625, -845.651977539063, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[216] := MakeD3DLVERTEX(-9045.083984375000, 454.914794921875, 2938.927490234375, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[217] := MakeD3DLVERTEX(-7694.208496093750, -938.926025390625, 2500.001220703125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[218] := MakeD3DLVERTEX(-7067.726074218750, 454.914794921875, 6363.811523437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[219] := MakeD3DLVERTEX(-6012.167480468750, -938.926025390625, 5413.381347656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[220] := MakeD3DLVERTEX(-3868.293701171875, 454.914794921875, 8688.333984375000, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[221] := MakeD3DLVERTEX(-3290.567382812500, -938.926025390625, 7390.738769531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[222] := MakeD3DLVERTEX(0.001662879949, 454.914794921875, 9510.564453125000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[223] := MakeD3DLVERTEX(0.001414530328, -938.926025390625, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[224] := MakeD3DLVERTEX(0.000000000000, -938.926025390625, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[225] := MakeD3DLVERTEX(0.000000000000, -2045.085205078125, 5877.852050781250, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[226] := MakeD3DLVERTEX(3290.568603515625, -938.926025390625, 7390.738281250000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[227] := MakeD3DLVERTEX(2390.737792968750, -2045.085205078125, 5369.684570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[228] := MakeD3DLVERTEX(6012.168457031250, -938.926025390625, 5413.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[229] := MakeD3DLVERTEX(4368.095214843750, -2045.085205078125, 3933.050292968750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[230] := MakeD3DLVERTEX(7694.208984375000, -938.926025390625, 2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[231] := MakeD3DLVERTEX(5590.169433593750, -2045.085205078125, 1816.356079101563, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[232] := MakeD3DLVERTEX(8045.851562500000, -938.926025390625, -845.653442382813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[233] := MakeD3DLVERTEX(5845.652343750000, -2045.085205078125, -614.403076171875, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[234] := MakeD3DLVERTEX(7006.292480468750, -938.926025390625, -4045.085693359375, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[235] := MakeD3DLVERTEX(5090.369140625000, -2045.085205078125, -2938.926269531250, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[236] := MakeD3DLVERTEX(4755.282226562500, -938.926025390625, -6545.085449218750, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[237] := MakeD3DLVERTEX(3454.914062500000, -2045.085205078125, -4755.282226562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[238] := MakeD3DLVERTEX(1682.040283203125, -938.926025390625, -7913.380859375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[239] := MakeD3DLVERTEX(1222.073608398438, -2045.085205078125, -5749.406738281250, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[240] := MakeD3DLVERTEX(-1682.041748046875, -938.926025390625, -7913.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[241] := MakeD3DLVERTEX(-1222.074707031250, -2045.085205078125, -5749.406250000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[242] := MakeD3DLVERTEX(-4755.283691406250, -938.926025390625, -6545.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[243] := MakeD3DLVERTEX(-3454.915283203125, -2045.085205078125, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[244] := MakeD3DLVERTEX(-7006.292968750000, -938.926025390625, -4045.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[245] := MakeD3DLVERTEX(-5090.369140625000, -2045.085205078125, -2938.925292968750, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[246] := MakeD3DLVERTEX(-8045.851562500000, -938.926025390625, -845.651977539063, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[247] := MakeD3DLVERTEX(-5845.652343750000, -2045.085205078125, -614.402099609375, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[248] := MakeD3DLVERTEX(-7694.208496093750, -938.926025390625, 2500.001220703125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[249] := MakeD3DLVERTEX(-5590.168945312500, -2045.085205078125, 1816.356933593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[250] := MakeD3DLVERTEX(-6012.167480468750, -938.926025390625, 5413.381347656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[251] := MakeD3DLVERTEX(-4368.094726562500, -2045.085205078125, 3933.051269531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[252] := MakeD3DLVERTEX(-3290.567382812500, -938.926025390625, 7390.738769531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[253] := MakeD3DLVERTEX(-2390.736816406250, -2045.085205078125, 5369.685058593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[254] := MakeD3DLVERTEX(0.001414530328, -938.926025390625, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[255] := MakeD3DLVERTEX(0.001027716207, -2045.085205078125, 5877.852050781250, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[256] := MakeD3DLVERTEX(0.000000000000, -2045.085205078125, 5877.852050781250, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[257] := MakeD3DLVERTEX(0.000000000000, -2755.283203125000, 3090.167968750000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[258] := MakeD3DLVERTEX(2390.737792968750, -2045.085205078125, 5369.684570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[259] := MakeD3DLVERTEX(1256.884521484375, -2755.283203125000, 2823.008789062500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[260] := MakeD3DLVERTEX(4368.095214843750, -2045.085205078125, 3933.050292968750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[261] := MakeD3DLVERTEX(2296.442382812500, -2755.283203125000, 2067.725830078125, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[262] := MakeD3DLVERTEX(5590.169433593750, -2045.085205078125, 1816.356079101563, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[263] := MakeD3DLVERTEX(2938.924316406250, -2755.283203125000, 954.914306640625, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[264] := MakeD3DLVERTEX(5845.652343750000, -2045.085205078125, -614.403076171875, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[265] := MakeD3DLVERTEX(3073.239746093750, -2755.283203125000, -323.010650634766, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[266] := MakeD3DLVERTEX(5090.369140625000, -2045.085205078125, -2938.926269531250, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[267] := MakeD3DLVERTEX(2676.163818359375, -2755.283203125000, -1545.084106445313, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[268] := MakeD3DLVERTEX(3454.914062500000, -2045.085205078125, -4755.282226562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[269] := MakeD3DLVERTEX(1816.354980468750, -2755.283203125000, -2499.998779296875, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[270] := MakeD3DLVERTEX(1222.073608398438, -2045.085205078125, -5749.406738281250, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[271] := MakeD3DLVERTEX(642.481811523438, -2755.283203125000, -3022.640380859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[272] := MakeD3DLVERTEX(-1222.074707031250, -2045.085205078125, -5749.406250000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[273] := MakeD3DLVERTEX(-642.482360839844, -2755.283203125000, -3022.640380859375, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[274] := MakeD3DLVERTEX(-3454.915283203125, -2045.085205078125, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[275] := MakeD3DLVERTEX(-1816.355468750000, -2755.283203125000, -2499.998291015625, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[276] := MakeD3DLVERTEX(-5090.369140625000, -2045.085205078125, -2938.925292968750, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[277] := MakeD3DLVERTEX(-2676.164306640625, -2755.283203125000, -1545.083618164063, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[278] := MakeD3DLVERTEX(-5845.652343750000, -2045.085205078125, -614.402099609375, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[279] := MakeD3DLVERTEX(-3073.239746093750, -2755.283203125000, -323.010131835938, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[280] := MakeD3DLVERTEX(-5590.168945312500, -2045.085205078125, 1816.356933593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[281] := MakeD3DLVERTEX(-2938.924072265625, -2755.283203125000, 954.914855957031, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[282] := MakeD3DLVERTEX(-4368.094726562500, -2045.085205078125, 3933.051269531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[283] := MakeD3DLVERTEX(-2296.442138671875, -2755.283203125000, 2067.726318359375, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[284] := MakeD3DLVERTEX(-2390.736816406250, -2045.085205078125, 5369.685058593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[285] := MakeD3DLVERTEX(-1256.884033203125, -2755.283203125000, 2823.009277343750, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[286] := MakeD3DLVERTEX(0.001027716207, -2045.085205078125, 5877.852050781250, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[287] := MakeD3DLVERTEX(0.000540302135, -2755.283203125000, 3090.167968750000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[288] := MakeD3DLVERTEX(0.000000000000, -2755.283203125000, 3090.167968750000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[289] := MakeD3DLVERTEX(0.000000000000, -3000.000000000000, -0.000874227786, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[290] := MakeD3DLVERTEX(1256.884521484375, -2755.283203125000, 2823.008789062500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[291] := MakeD3DLVERTEX(-0.000355580443, -3000.000000000000, -0.000798646768, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[292] := MakeD3DLVERTEX(2296.442382812500, -2755.283203125000, 2067.725830078125, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[293] := MakeD3DLVERTEX(-0.000649677881, -3000.000000000000, -0.000584972557, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[294] := MakeD3DLVERTEX(2938.924316406250, -2755.283203125000, 954.914306640625, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[295] := MakeD3DLVERTEX(-0.000831440033, -3000.000000000000, -0.000270151213, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[296] := MakeD3DLVERTEX(3073.239746093750, -2755.283203125000, -323.010650634766, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[297] := MakeD3DLVERTEX(-0.000869438692, -3000.000000000000, 0.000091381728, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[298] := MakeD3DLVERTEX(2676.163818359375, -2755.283203125000, -1545.084106445313, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[299] := MakeD3DLVERTEX(-0.000757103437, -3000.000000000000, 0.000437113922, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[300] := MakeD3DLVERTEX(1816.354980468750, -2755.283203125000, -2499.998779296875, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[301] := MakeD3DLVERTEX(-0.000513858104, -3000.000000000000, 0.000707265164, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[302] := MakeD3DLVERTEX(642.481811523438, -2755.283203125000, -3022.640380859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[303] := MakeD3DLVERTEX(-0.000181762109, -3000.000000000000, 0.000855123857, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[304] := MakeD3DLVERTEX(-642.482360839844, -2755.283203125000, -3022.640380859375, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[305] := MakeD3DLVERTEX(0.000181762269, -3000.000000000000, 0.000855123741, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[306] := MakeD3DLVERTEX(-1816.355468750000, -2755.283203125000, -2499.998291015625, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[307] := MakeD3DLVERTEX(0.000513858278, -3000.000000000000, 0.000707265048, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[308] := MakeD3DLVERTEX(-2676.164306640625, -2755.283203125000, -1545.083618164063, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[309] := MakeD3DLVERTEX(0.000757103437, -3000.000000000000, 0.000437113806, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[310] := MakeD3DLVERTEX(-3073.239746093750, -2755.283203125000, -323.010131835938, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[311] := MakeD3DLVERTEX(0.000869438692, -3000.000000000000, 0.000091381575, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[312] := MakeD3DLVERTEX(-2938.924072265625, -2755.283203125000, 954.914855957031, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[313] := MakeD3DLVERTEX(0.000831439975, -3000.000000000000, -0.000270151359, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[314] := MakeD3DLVERTEX(-2296.442138671875, -2755.283203125000, 2067.726318359375, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[315] := MakeD3DLVERTEX(0.000649677764, -3000.000000000000, -0.000584972673, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[316] := MakeD3DLVERTEX(-1256.884033203125, -2755.283203125000, 2823.009277343750, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[317] := MakeD3DLVERTEX(0.000355580356, -3000.000000000000, -0.000798646885, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[318] := MakeD3DLVERTEX(0.000540302135, -2755.283203125000, 3090.167968750000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[319] := MakeD3DLVERTEX(-0.000000000153, -3000.000000000000, -0.000874227786, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 1.000000000000);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TDXViewerForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  DXInput.Keyboard.Enabled := False;
  DXInput.Keyboard.BindInputStates := False;
  DXInput.Mouse.Enabled := False;
  DXInput.Mouse.BindInputStates := False;

  if WireFrame1.Checked then
    regWireFrame.Value := 1
  else
    regWireFrame.Value := 0;

  if ViewFloor1.Checked then
    regViewFloor.Value := 1
  else
    regViewFloor.Value := 0;

  if Fog1.Checked then
    regShowFog.Value := 1
  else
    regShowFog.Value := 0;

  regTextureFiltering.Value := doFiltering;

  if DisplayModeBox.ItemIndex >= 0 then
  begin
    regSceneWidth.Value :=
      (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).Width;
    regSceneHeight.Value :=
      (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).Height;
    regSceneBitCount.Value :=
      (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).BitCount;
  end;
  for i := 0 to DisplayModeBox.Items.Count - 1 do
    DisplayModeBox.Items.Objects[i].Free;
  DisplayModeBox.Items.Clear;

  Scene.Free;
  Scene := nil;

  DestroyComponents;
////////////////////////////////////////////////////////////////////////////////
// Sky support
  FreeMem(SPHERE_0004_ID_0008_Vertexes, 320 * SizeOf(TD3DLVertex));
////////////////////////////////////////////////////////////////////////////////
end;

procedure TDXViewerForm.FrameMovie(Time: Double);
const vPosition = 6;
      vRotation = 20;
var
  X, Y, Z,
  dX, dY, dZ: single;
begin

  if (Time <> oldTime) and // ΟΚ, we hope that we get a very very VERY big frame rate!!!!
     Active then
  begin
    { Κίνηση με τα βέλη σε FullScreen }
    X := Scene.Position.X;
    Y := Scene.Position.Y;
    Z := Scene.Position.Z;
    dX := Scene.Rotation.X;
    dY := Scene.Rotation.Y;
    dZ := Scene.Rotation.Z;


    if Floor <> nil then
      Floor.Culled := not ViewFloor1.Checked;

    DXInput.Update;
    if isLeft in DXInput.States then
      dY := dY - vPosition*((Time-oldTime) * vRotation * g_DEGTORAD)
    else if isRight in DXInput.States then
      dY := dY + vPosition*((Time-oldTime) * vRotation * g_DEGTORAD);

    if isButton1 in DXInput.States then
      dX := dX - vPosition*((Time-oldTime) * vRotation * g_DEGTORAD)
    else if isButton2 in DXInput.States then
      dX := dX + vPosition*((Time-oldTime) * vRotation * g_DEGTORAD);

    if isUp in DXInput.States then
    begin
    // ταχύτητα: vPosition μέτρα/sec,
    // όταν 1 μονάδα συντεταγμένωv D3D αντιστοιχεί σε ένα μέτρο πραγματικού κόσμου
      X := X + vPosition*Sin(dY)*(Time-oldTime);
      Z := Z + vPosition*cos(dY)*(Time-oldTime);
    end
    else if isDown in DXInput.States then
    begin
      X := X - vPosition*Sin(dY)*(Time-oldTime);
      Z := Z - vPosition*cos(dY)*(Time-oldTime);
    end;

    X := GetValueInRange(X, -200, 200);
    Z := GetValueInRange(Z, -200, 200);

    if isButton3 in DXInput.States then
      Y := Y + vPosition*(Time-oldTime)
    else if isButton4 in DXInput.States then
      Y := Y - vPosition*(Time-oldTime);
    Y := Max(FloorInfo.y1 + 1.0, Y);

    if dX < - g_PI_DIV_2 then
      dX := - g_PI_DIV_2
    else if dX > g_PI_DIV_2 then
      dX := g_PI_DIV_2;

    if not VectorEquel(Scene.Position, MakeD3DVECTOR(X, Y, Z)) then
      Scene.Position := MakeD3DVECTOR(X, Y, Z);

    Scene.Rotation := MakeD3DVECTOR(dX, dY, dZ);

  // Τελευταία στιγμή του D3D Render
    OldTime := Time;
  end;
end;

procedure TDXViewerForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//  Application Close Full Screen
  if Key = VK_ESCAPE then
  begin
    if (doFullScreen in DXDraw.Options) then
      AdjustFullScreen
  end
  else if (Key = Ord('F')) then
  begin
    Fog1Click(Sender);
  end
  else if (Key = Ord('W')) then
  begin
    WireFrame1Click(Sender);
  end;

//  Screen mode change
  if (ssAlt in Shift) and (Key = VK_RETURN) then AdjustFullScreen;
end;

procedure TDXViewerForm.ApplicationEvents1Activate(Sender: TObject);
begin
  if DXDraw.Visible then
  try
    DXDraw.Finalize;
    DXDraw.Initialize;
    if DXDraw.CanFocus then
      DXDraw.SetFocus;
    Scene.ForceReCalc;
  except
  end;
end;

procedure TDXViewerForm.DXDrawFinalize(Sender: TObject);
begin
//
end;

procedure TDXViewerForm.AdjustFullScreen(const rlevel: integer = 0);
begin
  if rlevel > 0 then
  begin
    if rlevel > 1 then
      Exit;
    if not (doFullScreen in DXDraw.Options) then
      Exit;
  end;

  DXDraw.Finalize;
  if (doFullScreen in DXDraw.Options) then
  begin
    RestoreWindow;
    Visible := True;
    DXDraw.Cursor := crDefault;
    Screen.Cursor := crDefault;
    Menu := MainMenu1;
    BorderStyle := bsSizeable;
    DXDraw.Options := DXDraw.Options - [doFullScreen] + [doFlip];
    TryFocusControl(DXDraw);
    ShowHint := True;
    WIN_EnableAltTab;
  end
  else
  begin
    WIN_DisableAltTab;
    if DisplayModeBox.ItemIndex >= 0 then
    begin
      DXDraw.Display.Width :=
        (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).Width;
      DXDraw.Display.Height :=
        (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).Height;
      DXDraw.Display.BitCount :=
        (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).BitCount;
    end
    else
    begin
      DXDraw.Display.Width := DefSceneWidth;
      DXDraw.Display.Height := DefSceneHeight;
      DXDraw.Display.BitCount := DefSceneBitCount;
    end;
    ShowHint := False;
    TryFocusControl(DXDraw);
    StoreWindow;
    Visible := False;
    Menu := nil;
    DXDraw.Cursor := crNone;
    Screen.Cursor := crNone;
    BorderStyle := bsNone;
    DXDraw.Options := DXDraw.Options + [doFullScreen] - [doFlip];
  end;
  try
    DXDraw.Initialize;
  except
    on E : Exception do
    begin
      serrormessage := E.ClassName + '() : ' + E.Message;
      AdjustFullScreen(rlevel + 1);
      Exit;
    end;
  end;
  Scene.ForceReCalc;
  Scene.ReInitialize;
end;

procedure TDXViewerForm.About1Click(Sender: TObject);
begin
  AboutDialog1.Execute
end;

procedure TDXViewerForm.LoadMap;
var
  cs: TCriticalSection;
  Actor: TD3DObject;
begin
  cs := TCriticalSection.Create;
  Screen.Cursor := crHourglass;
  isLoading := True;
  try
    cs.Enter;
    Scene.New;
    Actor := Scene.AddSurface(ID3D_ACTOR, @Info);
    if Actor <> nil then
    begin
      FloorInfo.y1 := ((Actor as TD3DActor).Model.Center.y - (Actor as TD3DActor).Model.Radius) * (Actor as TD3DActor).AppliedInfo.Scale;
      FloorInfo.y2 := FloorInfo.y1;
      FloorInfo.y3 := FloorInfo.y1;
      FloorInfo.y4 := FloorInfo.y1;
      Floor := Scene.AddSurface(ID3D_QUADRANGLE, @FloorInfo);
    end;
    Caption := Format(rsFmtTitle, [MkShortName(Info.PathToModel)]);
    AdjustFog;
    AdjustFocus;
  finally
    isLoading := False;
    cs.Release;
    cs.Free;
    Screen.Cursor := crDefault;
  end;
end;


procedure TDXViewerForm.OpenClick(Sender: TObject);
var s: TStringList;
    idx: integer;
begin
  s := TStringList.Create;
  try
    if QueryImportQuakeModel(Info, s, idx) then
    begin
      Info.TextureNames[0, 1] := rsBlack;
      NextMap.Visible := False;
      ComboBox1.Visible := False;
      PrevMap.Visible := False;
      ComboBox1.Items.Clear;
      PrevMap.Visible := True;
      ComboBox1.Visible := True;
      NextMap.Visible := True;
      ComboBox1.Items.AddStrings(s);
      ComboBox1.ItemIndex := idx;
//      ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(Info.);
      UpdateModelFrameNames;
      ComboBox1.Enabled := ComboBox1.Items.Count > 0;
      PrevMap.Enabled := ComboBox1.Enabled and (ComboBox1.ItemIndex > 0);
      NextMap.Enabled := ComboBox1.Enabled and (ComboBox1.ItemIndex <> ComboBox1.Items.Count - 1);
      LoadMap;
      ResetPosition;
    end;
  finally
    s.Free;
  end;
end;

procedure TDXViewerForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TDXViewerForm.FullScreenClick(Sender: TObject);
begin
  AdjustFullScreen;
end;

procedure TDXViewerForm.DisplayModeBoxChange(Sender: TObject);
begin
  AdjustFocus
end;

procedure TDXViewerForm.PrevMapClick(Sender: TObject);
begin
  if ComboBox1.ItemIndex > 0 then
  begin
    ComboBox1.ItemIndex := ComboBox1.ItemIndex - 1;
    PrevMap.Enabled := ComboBox1.ItemIndex > 0;
    NextMap.Enabled := True;
    ComboBox1.Enabled := True;
    UpdateModelFrameNames;
    LoadMap;
  end;
end;

procedure TDXViewerForm.NextMapClick(Sender: TObject);
begin
  if ComboBox1.ItemIndex < ComboBox1.Items.Count - 1 then
  begin
    ComboBox1.ItemIndex := ComboBox1.ItemIndex + 1;
    PrevMap.Enabled := True;
    NextMap.Enabled := ComboBox1.ItemIndex < ComboBox1.Items.Count - 1;
    ComboBox1.Enabled := True;
    UpdateModelFrameNames;
    LoadMap;
  end;
end;

procedure TDXViewerForm.ComboBox1Change(Sender: TObject);
begin
  UpdateModelFrameNames;
  LoadMap;
  AdjustFocus;
end;

procedure TDXViewerForm.CopyClick(Sender: TObject);
var
  aBitmap: TBitmap;
  r: TRect;
begin
  aBitmap := TBitmap.Create;
  try
    aBitmap.Width := DXDraw.Width;
    aBitmap.Height := DXDraw.Height;
    if aBitmap.Width * aBitmap.Height <> 0 then
    begin
      SetRect(r, 0, 0, aBitmap.Width, aBitmap.Height);
      aBitmap.Canvas.CopyRect(r, DXDraw.Surface.Canvas, r);
      DXDraw.Surface.Canvas.Release;
      Clipboard.Assign(aBitmap);
    end
    else
      ClipboardErrorMessageBox.Execute;
  finally
    aBitmap.Free;
  end;
end;

procedure TDXViewerForm.ApplicationEvents1Hint(Sender: TObject);
begin
  if Trim(Application.Hint) = EmptyStr then
    StatusBar1.Panels[0].Text := ' ' + Application.Title
  else
    StatusBar1.Panels[0].Text := ' ' + Trim(Application.Hint);
end;

resourceString
  rsMailTo = 'mailto';
  rsJimmyValavanis = 'jimmyvalavanis';
  rsProvider = 'yahoo.gr';
  rsSubject = 'subject';
  rsFmtMail = '%s:%s@%s?%s=%s';

procedure TDXViewerForm.Contactme1Click(Sender: TObject);
begin
  ShellExecute(
    handle,
      PChar(rsOpen),
        PChar(Format(rsFmtMail, [rsMailTo, rsJimmyValavanis, rsProvider, rsSubject, Application.Title])),
          nil, nil, SW_SHOWNORMAL);
end;

procedure TDXViewerForm.AdjustFocus;
begin
  if Visible then
    if DXDraw.CanFocus then
      DXDraw.SetFocus
end;

procedure TDXViewerForm.FormShow(Sender: TObject);
begin
  AdjustFocus;
end;

procedure TDXViewerForm.ApplicationEvents1Deactivate(Sender: TObject);
begin
//
end;

procedure TDXViewerForm.QuickInfo1Click(Sender: TObject);
begin
  with TQuickInfoForm.Create(nil) do
  try
    ShowModal
  finally
    Free;
  end;
end;

procedure TDXViewerForm.Homepage1Click(Sender: TObject);
begin
  ShellExecute(
    handle,
      PChar(rsOpen),
        PChar(rsHomePage),
          nil, nil, SW_SHOWNORMAL);
end;

resourceString
  rsExtPNG = '.png';
  rsExtJPG1 = '.jpg';
  rsExtJPG2 = '.jpeg';
  rsExtBMP = '.bmp';
  rsExtPPM = '.ppm';
  rsExtM8 =  '.m8';
  rsExtTGA = '.tga';

procedure TDXViewerForm.Save2Click(Sender: TObject);
var
  ext: string;
  aBitmap: TBitmap;
  aPNG: TPNGObject;
  aJPG: TJpegImage;
  r: TRect;
begin
  SavePictureDialog1.FileName := '';
  if SavePictureDialog1.Execute then
  begin
    Screen.Cursor := crHourglass;
    try
      ext := ExtractFileExt(SavePictureDialog1.Filename);
      aBitmap := nil;
      if ext = '' then
      begin
        case SavePictureDialog1.FilterIndex of
          1: ext := rsExtPNG;
          2: ext := rsExtJPG1;
          3: ext := rsExtBMP;
          4: ext := rsExtPPM;
          5: ext := rsExtM8;
          6: ext := rsExtTGA;
        else
          begin
            MessageBox1.Execute;
            exit;
          end;
        end;
        SavePictureDialog1.Filename := SavePictureDialog1.Filename + ext;
      end;
      if (UpperCase(ext) = UpperCase(rsExtPNG)) or
         (UpperCase(ext) = UpperCase(rsExtJPG1)) or
         (UpperCase(ext) = UpperCase(rsExtJPG2)) then
      begin
        aBitmap := TTGABitmap.Create;
        if Assigned(aBitmap) then
        begin
          try
            aBitmap.Width := DXDraw.Width;
            aBitmap.Height := DXDraw.Height;
            if aBitmap.Width * aBitmap.Height <> 0 then
            begin
              aBitmap.PixelFormat := pf32bit;
              SetRect(r, 0, 0, aBitmap.Width, aBitmap.Height);
              aBitmap.Canvas.CopyRect(r, DXDraw.Surface.Canvas, r);
              DXDraw.Surface.Canvas.Release;
            end;
            if UpperCase(ext) = UpperCase(rsExtPNG) then
            begin
              aPNG := TPNGObject.Create;
              try
                aPNG.Assign(aBitmap);
                CreateBackupFile(SavePictureDialog1.Filename);
                aPNG.SaveToFile(SavePictureDialog1.Filename);
              finally
                aPNG.Free;
              end;
            end
            else
            begin
              aJPG := TJpegImage.Create;
              try
                aJPG.Assign(aBitmap);
                CreateBackupFile(SavePictureDialog1.Filename);
                aJPG.SaveToFile(SavePictureDialog1.Filename);
              finally
                aJPG.Free;
              end;
            end;
          finally
            aBitmap.Free;
          end;
        end
        else
          MessageBox1.Execute;
      end
      else
      begin
        if UpperCase(ext) = UpperCase(rsExtBMP) then
          aBitmap := TBitmap.Create
        else if UpperCase(ext) = UpperCase(rsExtM8) then
          aBitmap := TM8Bitmap.Create
        else if UpperCase(ext) = UpperCase(rsExtPPM) then
          aBitmap := TPPMBitmap.Create
        else if UpperCase(ext) = UpperCase(rsExtTGA) then
          aBitmap := TTGABitmap.Create;
        if Assigned(aBitmap) then
        begin
          try
            aBitmap.Width := DXDraw.Width;
            aBitmap.Height := DXDraw.Height;
            if aBitmap.Width * aBitmap.Height <> 0 then
            begin
              SetRect(r, 0, 0, aBitmap.Width, aBitmap.Height);
              aBitmap.Canvas.CopyRect(r, DXDraw.Surface.Canvas, r);
              DXDraw.Surface.Canvas.Release;
            end;
            CreateBackupFile(SavePictureDialog1.Filename);
            aBitmap.SaveToFile(SavePictureDialog1.Filename);
          finally
            aBitmap.Free;
          end;
        end
        else
          MessageBox1.Execute;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TDXViewerForm.UpdateModelFrameNames;
var
  f: TFileStream;
  mdl: TModel;
  mv: string;
  idx: integer;
begin
  if (ComboBox1.ItemIndex > -1) and (Info.PathToModel <> '') then
  begin
    mv := ComboBox1.Items[ComboBox1.ItemIndex];
    if FileExists(Info.PathToModel) then
    begin
      f := TFileStream.Create(Info.PathToModel, fmOpenRead or fmShareDenyWrite);
      Screen.Cursor := crHourglass;
      try
        if UpperCase(ExtractFileExt(Info.PathToModel)) = UpperCase(rsExtMD2) then
        begin
          mdl := TModelMD2.Create(Info.PathToModel, f);
          try
            idx := mdl.frameNames.IndexOf(mv);
            if idx > -1 then
            begin
              if Info.AnimStart < Info.AnimEnd then
                Info.AnimSpeed :=
                  Info.AnimSpeed * (mdl.EndFrame(idx) - mdl.StartFrame(idx)) /
                                   (Info.AnimEnd - Info.AnimStart)
              else
                Info.AnimSpeed := (mdl.EndFrame(idx) - mdl.StartFrame(idx)) / 5;
              Info.AnimStart := mdl.StartFrame(idx);
              Info.AnimEnd := mdl.EndFrame(idx);
            end;
          finally
            mdl.Free;
          end;
        end
{        else if UpperCase(ExtractFileExt(ModelEdit.Text)) = UpperCase(rsExtVRT) then
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
        end;}
      finally
        f.Free;
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TDXViewerForm.ResetPositionButtonClick(Sender: TObject);
begin
  ResetPosition;
end;

procedure TDXViewerForm.ResetPosition;
begin
  Scene.ForcePosition(MakeD3DVector(0, 0, 0));
  Scene.ForceRotation(MakeD3DVector(0, 0, 0));
end;

procedure TDXViewerForm.ViewFloor1Click(Sender: TObject);
begin
  ViewFloor1.Checked := not ViewFloor1.Checked;
end;

procedure TDXViewerForm.DXDrawClick(Sender: TObject);
begin
  AdjustFocus;
end;

procedure TDXViewerForm.AdjustFog;
begin
  if Scene <> nil then
  begin
    if Fog1.Checked then
      Scene.StartFog($D0D0D0, 255, 50.0, 150.0, 0.001)
    else
      Scene.StopFog;
  end;
end;

procedure TDXViewerForm.Fog1Click(Sender: TObject);
begin
  Fog1.Checked := not Fog1.Checked;
  AdjustFog;
end;

procedure TDXViewerForm.Wireframe1Click(Sender: TObject);
begin
  Wireframe1.Checked := not Wireframe1.Checked;
end;

procedure TDXViewerForm.TextureFiltering1Click(Sender: TObject);
begin
  TextureFiltering1.Checked := not TextureFiltering1.Checked;
  doFiltering := TextureFiltering1.Checked;
  regTextureFiltering.Value := doFiltering;
end;

end.
