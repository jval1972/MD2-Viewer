//------------------------------------------------------------------------------
//
//  Surfaces Engine (SE) - Gaming engine for Windows based on DirectX & DelphiX
//  Copyright (C) 1999-2004, 2018 by Jim Valavanis
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
//  3D Model Classes & definitions
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

{$I defs.inc}

unit se_DXModels;

interface

uses
  Windows, Classes, se_DirectX, se_MyD3DUtils;

const
  ERR_MODEL_NONE = 0;
  ERR_MODEL_LOADING = 1;
  ERR_MODEL_INVALIDFRAME = 2;

//------------------------------------------------------------------------------
//--------------------------- Base Model Class ---------------------------------
//------------------------------------------------------------------------------

type
  TFrameIndexInfo = class(TObject)
  private
    fStartFrame,
    fEndFrame: integer;
  public
    property StartFrame: integer read fStartFrame write fStartFrame;
    property EndFrame: integer read fEndFrame write fEndFrame;
  end;

  TModel = class(TObject)
  protected
    fNumFrames: integer;
    fNumVertexes: integer;
    fName: string;
    fLastError: integer;
    fCenter: TD3DVector;
    fRadius: TD3DValue;
    procedure BuildList; virtual;
    procedure BuildFrame(frm: integer); virtual;
  public
    frameNames: TStringList;
    TheVectorsArray: PD3DVectorArrays;
    UV: PD3DuvArray;
    constructor Create(aName: string; aStream : TStream); virtual;
    function LoadFromStream(aStream: TStream): boolean; virtual;
    destructor Destroy; override;
    // Allocates VR pointer to size of vertexes
    function AllocFrameVertexes(frm: integer; var VR: PD3DLVertexArray): integer; overload; virtual;
    function AllocFrameVertexes(const frm: TD3DValue;
      var VR: PD3DLVertexArray; const sFrame, eFrame: word): integer; overload; virtual;
    // Does not Allocates VR pointer to size of vertexes
    function GetFrameVertexes(const frm: integer; VR: PD3DLVertexArray): integer; overload; virtual;
    function GetFrameVertexes(const frm: TD3DValue;
      VR: PD3DLVertexArray; const sFrame, eFrame: word): integer; overload; virtual;
    // Same as GetFrameVertexes but with out color, specular & u, v
    function GetFrameVertexesOnlyXYZ(const frm: integer; VR: PD3DLVertexArray): integer; overload; virtual;
    function GetFrameVertexesOnlyXYZ(const frm: TD3DValue;
      VR: PD3DLVertexArray; const sFrame, eFrame: word; tolerance: TD3DValue = 0.01): integer; overload; virtual;
    function GetFrameVertexesOnlyXYZ(const frm1, frm2: TD3DValue;
      VR: PD3DLVertexArray): integer; overload; virtual;

    function StartFrame(const i: integer): integer; overload; virtual;
    function EndFrame(const i: integer): integer; overload; virtual;
    function StartFrame(const frame: string): integer; overload; virtual;
    function EndFrame(const frame: string): integer; overload; virtual;
    function FrameName(const i: integer): string; virtual;
    function FrameIndex(const frame: string): integer; virtual;

    property NumFrames: integer read fNumFrames;
    property NumVertexes: integer read fNumVertexes;
    property Name: string read fName;
    property LastError: integer read fLastError;
    property Center: TD3DVector read fCenter;
    property Radius: TD3DValue read fRadius;
  end;

//------------------------------------------------------------------------------

  TVec3_T = record
    V: array[0..2] of Single;
  end;

//------------------------------------------------------------------------------
//--------------------------- MDL File Format ----------------------------------
//------------------------------------------------------------------------------

const
  // Magic number that identifies MDL files (ASCII: 'IDPO')
  MDL_MAGIC = 1330660425;

type
  TDmdl_T = record
    ident: longint;
    version: longint;
    scale: TVec3_T;
    scale_origin: TVec3_T;
    boundingradius: double;
    eyeposition: TVec3_T;
    numskins: longint;
    skinwidth: longint;
    skinheight: longint;
    numverts: longint;
    numtris: longint;
    numframes: longint;
    synctype: longint;
    flags: longint;
    size: double;
  end;

  TMDLDstVert_T = record
    onseam: longint;
    s: longint;
    t: longint;
  end;

  TMDLTriangle_T = record
    facesfront : longint;
    vertindex : array[0..2] of longint;
  end;

  TMDLTrivertx_T = record
    v: array[0..2] of Byte;
    lightnormalindex: byte;
  end;

  TMDLAliasFrame_T = record
    bboxmin: TMDLTrivertx_T;
    bboxmax : TMDLTrivertx_T;
    name: array[0..15] of char;
  end;


//------------------------------------------------------------------------------
//--------------------------- MD2 File Format ----------------------------------
//------------------------------------------------------------------------------

const
  MD2_MAX_VERTS = 2048;
  MD2_MAX_SKINS = 32;
  MD2_MAX_SKINNAME = 64;

const
  // Magic number that identifies MD2 files (ASCII: 'IDP2').
  MD2_MAGIC = $32504449;

type
  TMD2_Index_List = record
    a, b, c: Integer;
    a_s, a_t,
    b_s, b_t,
    c_s, c_t: Single;
  end;
  TMD2_Index_List_Array = array[0..$FFFF] of TMD2_Index_List;
  PMD2_Index_List = ^TMD2_Index_List_Array;

  TMD2_Vertex_List = record
    x, y, z: Single;
  end;
  TMD2_Vertex_List_Array = array[0..$FFFF] of TMD2_Vertex_List;
  PMD2_Vertex_List = ^TMD2_Vertex_List_Array;

  TMD2_Frame_List = record
    Vertex: PMD2_Vertex_List;
  end;
  TMD2_Frame_List_Array = array[0..$FFFF] of TMD2_Frame_List;
  PMD2_Frame_List = ^TMD2_Frame_List_Array;

  TMD2DstVert_T = record
    s: SmallInt;
    t: SmallInt;
  end;

  TMD2Triangle_T = record
    index_xyz: array[0..2] of SmallInt;
    index_st: array[0..2] of SmallInt;
  end;

  TMD2Trivertx_T = record
    v: array[0..2] of Byte;
    lightnormalindex: byte;
  end;

  PMD2AliasFrame_T = ^TMD2AliasFrame_T;
  TMD2AliasFrame_T = record
    scale: array[0..2] of Single;
    translate: array[0..2] of Single;
    name: array[0..15] of Char;
    verts: array[0..0] of TMD2Trivertx_T;
  end;

  TDmd2_T = record
    ident: Integer;
    version: Integer;

    skinwidth: Integer;
    skinheight: Integer;
    framesize: Integer;

    num_skins: Integer;
    num_xyz: Integer;
    num_st: Integer;
    num_tris: Integer;
    num_glcmds: Integer;
    num_frames: Integer;

    ofs_skins: Integer;
    ofs_st: Integer;
    ofs_tris: Integer;
    ofs_frames: Integer;
    ofs_glcmds: Integer;
    ofs_end: Integer;
  end;

  TModelMD2 = class(TModel)
  protected
    fInternalListsValid: boolean;
    m_index_list: PMD2_Index_List;
    m_frame_list: PMD2_Frame_List;
    fm_iTriangles: integer;
    procedure BuildList; override;
    procedure DisposeLists; virtual;
    procedure BuildFrame(frm: integer); override;
  public
    constructor Create(aName: string; aStream : TStream); override;
    destructor Destroy; override;
    function LoadFromStream(aStream: TStream): boolean; override;
  end;

//------------------------------------------------------------------------------
//--------------------------- MD3 File Format ----------------------------------
//------------------------------------------------------------------------------

type
  MD3HEADER = packed record
  	ID: packed array[0..3] of char;
    nVersion: integer;
    Filename: packed array[0..63] of char;
    nBoneFrameNum: integer;
    nTagNum: integer;
    nMeshNum: integer;
    nMaxSkinNum: integer;
    nHeaderLenght: integer;
    nTagStart: integer;
    nTagEnd: integer;
    nFilesize: integer;
  end;

  MD3TAG = packed record
    Name: packed array[0..63] of char;
    Position: packed array[0..2] of single;
    Rotation: packed array[0..2, 0..2] of single;
  end;

  MD3BONEFRAME = packed record
  	Max: packed array[0..2] of single;
    Min: packed array[0..2] of single;
    Position: packed array[0..2] of single;
    scale: single;
    Creator: packed array[0..15] of char;
  end;

  MD3VERTICE = packed record
  	nVec: packed array[0..2] of smallint;
    EnvTex: packed array[0..1] of byte;
  end;

  MD3TRIANGLE = packed record
  	nVertices: array[0..2] of integer;
  end;

  MD3TEXTURE = packed record
  	fTexture: packed array[0..1] of single;
  end;

  MD3MESH = record
    ID: packed array[0..3] of char;
    Name: packed array[0..67] of char;
    nFrameNum: integer;
    nSkinNum: integer;
    nVertexNum: integer;
    nTriangleNum: integer;
    nTriangleStart: integer;
    nHeaderSize: integer;
    nTexVecStart: integer;
    nVertexStart: integer;
    nMeshSize: integer;
//  char  Data[0x10000];
//  unsigned int nTexture;
  end;

//------------------------------------------------------------------------------
//--------------------------- VRT File Format ----------------------------------
//------------------------------------------------------------------------------

  TModelVRT = class(TModel)
  protected
    fStream: TStream;
    fNumLVertexes: integer;
    fPrimitiveType: TD3DPrimitiveType;
    procedure BuildFrame(frm: integer); override;
  public
    constructor Create(aName: string; aStream : TStream); override;
    function LoadFromStream(aStream: TStream): boolean; override;
  end;

//------------------------------------------------------------------------------
//--------------------------- VRM File Format ----------------------------------
//------------------------------------------------------------------------------

resourceString
  rsExtVRM = '.vrm';
  rsVrmMagic = 'VRM';

type
  TModelVRM = class(TModel)
  private
    strmPos: integer;
  protected
    fStream: TStream;
    fNumLVertexes: integer;
    fPrimitiveType: TD3DPrimitiveType;
    procedure BuildFrame(frm: integer); override;
  public
    constructor Create(aName: string; aStream : TStream); override;
    function LoadFromStream(aStream: TStream): boolean; override;
  end;

//------------------------------------------------------------------------------
//---------------------------- X File Format -----------------------------------
//------------------------------------------------------------------------------

{$IFNDEF NO_MODELX}
resourceString
  rsExtX = '.x';

type
  TModelX = class(TModel)
  protected
    fStream: TStream;
    fNeedsStreamFree: boolean;
    procedure DoCleanUp; virtual;
    procedure BuildFrame(frm: integer); override;
  public
    constructor Create(aName: string; aStream : TStream); override;
    destructor Destroy; override;
    function LoadFromStream(aStream: TStream): boolean; override;
  end;
{$ENDIF}
function ModelErrorToString(err: integer): string;

implementation

uses
  SysUtils, Math, se_D3DUtils, se_DXDUtils;

var D3DRM3: IDirect3DRM3;

function GetD3DRM: IDirect3DRM3;
var tmpD3DRM: IDirect3DRM;
begin
  if D3DRM3 = nil then
  begin
    Direct3DRMCreate(tmpD3DRM);
    tmpD3DRM.QueryInterface(IID_IDirect3DRM3, D3DRM3);
    tmpD3DRM._Release;
  end;
  result := D3DRM3;
end;

//------------------------------------------------------------------------------
//--------------------------- Base Model Class ---------------------------------
//------------------------------------------------------------------------------

constructor TModel.Create(aName: string; aStream : TStream);
var oldPos: integer;
begin
  Inherited Create;
  fCenter := NULLVECTOR;
  fRadius := 0.0;
  fLastError := ERR_MODEL_NONE;
  frameNames := TStringList.Create;
  UV := nil;
  TheVectorsArray := nil;
  fName := aName;
  fNumFrames := 0;
  fNumVertexes := 0;
  if aStream <> nil then
  begin
    oldPos := aStream.Position;
    if not LoadFromStream(aStream) then
      fLastError := ERR_MODEL_LOADING;
    aStream.Position := oldPos;
  end;
  BuildList;
end;

//------------------------------------------------------------------------------

destructor TModel.Destroy;
var i: integer;
begin
  FreeMem(UV, fNumVertexes * SizeOf(TD3Duv));
  for i := 0 to fNumFrames - 1 do
    FreeMem(TheVectorsArray[i], fNumVertexes * SizeOf(TD3DVector));
  FreeMem(TheVectorsArray, fNumFrames * SizeOf(PD3DVectorArray));

  for i := 0 to frameNames.Count - 1 do
    frameNames.Objects[i].Free;
  frameNames.Free;
  Inherited Destroy;
end;

//------------------------------------------------------------------------------

function TModel.LoadFromStream(aStream: TStream): boolean;
begin
  result := false;
end;

//------------------------------------------------------------------------------

procedure TModel.BuildList;
var i: integer;
begin
  ReAllocMem(UV, fNumVertexes * SizeOf(TD3Duv));
  ReAllocMem(TheVectorsArray, fNumFrames * SizeOf(PD3DVectorArray));
  for i := 0 to fNumFrames - 1 do
  begin
    TheVectorsArray[i] := nil;
    ReAllocMem(TheVectorsArray[i], fNumVertexes * SizeOf(TD3DVector));
    BuildFrame(i);
  end;
  if fNumFrames * fNumVertexes <> 0 then
    CalcCenterAndRadius(TheVectorsArray[0], fNumVertexes, fRadius, fCenter);
end;

//------------------------------------------------------------------------------

procedure TModel.BuildFrame(frm: integer);
begin
end;

//------------------------------------------------------------------------------

function TModel.AllocFrameVertexes(frm: integer; var VR: PD3DLVertexArray): integer;
var i: integer;
begin
  if IsIntegerInRange(frm, 0, fNumFrames - 1) then
  begin
    ReAllocMem(VR, fNumVertexes * SizeOf(TD3DLVertex));
    for i := 0 to fNumVertexes - 1 do
      VR[i] := MakeD3DLVertex(TheVectorsArray[frm][i], UV[i]);
    result := fNumVertexes;
  end
  else
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
  end;
end;

//------------------------------------------------------------------------------

function TModel.AllocFrameVertexes(const frm: TD3DValue;
  var VR: PD3DLVertexArray; const sFrame, eFrame: word): integer;
var i: integer;
    i1, i2: integer;
    w2: TD3DValue;
begin
  if not IsFloatInRange(frm, sFrame, eFrame + 1) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;
  if not IsIntegerInRange(sFrame, 0, fNumFrames - 1) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;
  if not IsIntegerInRange(eFrame, 0, fNumFrames - 1) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;

  i1 := trunc(frm);
  i2 := i1 + 1;
  if i2 > eFrame then i2 := sFrame;
  w2 := frm - i1;

  ReAllocMem(VR, fNumVertexes * SizeOf(TD3DLVertex));
  for i := 0 to fNumVertexes - 1 do
    VR[i] := MakeD3DLVertex(TheVectorsArray[i1][i], TheVectorsArray[i2][i], w2, UV[i]);
  result := fNumVertexes;

end;

//------------------------------------------------------------------------------

function TModel.GetFrameVertexes(const frm: integer; VR: PD3DLVertexArray): integer;
// Same as AllocFrameVertexes but does not allocate VR pointer
var i: integer;
begin
  if IsIntegerInRange(frm, 0, fNumFrames - 1) then
  begin
    for i := 0 to fNumVertexes - 1 do
      VR[i] := MakeD3DLVertex(TheVectorsArray[frm][i], UV[i]);
    result := fNumVertexes;
  end
  else
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
  end;
end;

//------------------------------------------------------------------------------

function TModel.GetFrameVertexes(const frm: TD3DValue;
  VR: PD3DLVertexArray; const sFrame, eFrame: word): integer;
var i: integer;
    i1, i2: integer;
    w2: TD3DValue;
begin
  if not IsFloatInRange(frm, sFrame, eFrame + 1) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;
  if not IsIntegerInRange(sFrame, 0, fNumFrames - 1) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;
  if not IsIntegerInRange(eFrame, 0, fNumFrames - 1) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;

  i1 := trunc(frm);
  i2 := i1 + 1;
  if i2 > eFrame then i2 := sFrame;
  w2 := frm - i1;

  if w2 <= 0.01 then
  begin
    for i := 0 to fNumVertexes - 1 do
    begin
      VR[i].x := TheVectorsArray[i1][i].x;
      VR[i].y := TheVectorsArray[i1][i].y;
      VR[i].z := TheVectorsArray[i1][i].z;
      VR[i].tu := UV[i].u;
      VR[i].tv := UV[i].v;
    end
  end
  else if w2 >= 0.99 then
  begin
    for i := 0 to fNumVertexes - 1 do
    begin
      VR[i].x := TheVectorsArray[i2][i].x;
      VR[i].y := TheVectorsArray[i2][i].y;
      VR[i].z := TheVectorsArray[i2][i].z;
      VR[i].tu := UV[i].u;
      VR[i].tv := UV[i].v;
    end
  end
  else for i := 0 to fNumVertexes - 1 do
    VR[i] := MakeD3DLVertex(TheVectorsArray[i1][i], TheVectorsArray[i2][i], w2, UV[i]);

  result := fNumVertexes;

end;

//------------------------------------------------------------------------------

function TModel.GetFrameVertexesOnlyXYZ(const frm: integer; VR: PD3DLVertexArray): integer;
var i: integer;
begin
  if IsIntegerInRange(frm, 0, fNumFrames - 1) then
  begin
    for i := 0 to fNumVertexes - 1 do
    begin
      VR[i].x := TheVectorsArray[frm][i].x;
      VR[i].y := TheVectorsArray[frm][i].y;
      VR[i].z := TheVectorsArray[frm][i].z;
    end;
    result := fNumVertexes;
  end
  else
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
  end;
end;

//------------------------------------------------------------------------------

function TModel.GetFrameVertexesOnlyXYZ(const frm: TD3DValue;
      VR: PD3DLVertexArray; const sFrame, eFrame: word; tolerance: TD3DValue = 0.01): integer;
var i: integer;
    i1, i2: integer;
    w1, w2: TD3DValue;
begin
  if not IsFloatInRange(frm, sFrame, eFrame + 1) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;
  if not IsIntegerInRange(sFrame, 0, fNumFrames - 1) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;
  if not IsIntegerInRange(eFrame, 0, fNumFrames - 1) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;

  i1 := trunc(frm);
  i2 := i1 + 1;
  if i2 > eFrame then i2 := sFrame;
  w2 := frm - i1;
  w1 := 1.0 - w2;

  if w2 <= tolerance then
  begin
    for i := 0 to fNumVertexes - 1 do
    begin
      VR[i].x := TheVectorsArray[i1][i].x;
      VR[i].y := TheVectorsArray[i1][i].y;
      VR[i].z := TheVectorsArray[i1][i].z;
    end
  end
  else if w1 <= tolerance then
  begin
    for i := 0 to fNumVertexes - 1 do
    begin
      VR[i].x := TheVectorsArray[i2][i].x;
      VR[i].y := TheVectorsArray[i2][i].y;
      VR[i].z := TheVectorsArray[i2][i].z;
    end
  end
  else
  begin
    for i := 0 to fNumVertexes - 1 do
    begin
      VR[i].x := TheVectorsArray[i1][i].x * w1 + TheVectorsArray[i2][i].x * w2;
      VR[i].y := TheVectorsArray[i1][i].y * w1 + TheVectorsArray[i2][i].y * w2;
      VR[i].z := TheVectorsArray[i1][i].z * w1 + TheVectorsArray[i2][i].z * w2;
    end;
  end;
  result := fNumVertexes;

end;

//------------------------------------------------------------------------------
// Interpolate between float frames frm1, frm2
function TModel.GetFrameVertexesOnlyXYZ(const frm1, frm2: TD3DValue;
  VR: PD3DLVertexArray): integer;
var i: integer;
    i1, i2: integer;
    w1, w2: TD3DValue;
begin
  if not IsFloatInRange(frm1, 0, fNumFrames) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;
  if not IsFloatInRange(frm2, 0, fNumFrames) then
  begin
    fLastError := ERR_MODEL_INVALIDFRAME;
    result := -1;
    exit;
  end;

  i1 := trunc(frm1);
  i2 := trunc(frm2);
  w2 := 0.5;
  w1 := 0.5;

  for i := 0 to fNumVertexes - 1 do
  begin
    VR[i].x := TheVectorsArray[i1][i].x * w1 + TheVectorsArray[i2][i].x * w2;
    VR[i].y := TheVectorsArray[i1][i].y * w1 + TheVectorsArray[i2][i].y * w2;
    VR[i].z := TheVectorsArray[i1][i].z * w1 + TheVectorsArray[i2][i].z * w2;
  end;

  result := fNumVertexes;

end;

//------------------------------------------------------------------------------

function TModel.StartFrame(const i: integer): integer;
begin
  result := -1;
  if IsIntegerInRange(i, 0, frameNames.Count - 1) then
    if frameNames.Objects[i] <> nil then
      result := (frameNames.Objects[i] as TFrameIndexInfo).StartFrame;
end;

//------------------------------------------------------------------------------

function TModel.EndFrame(const i: integer): integer;
begin
  result := -1;
  if IsIntegerInRange(i, 0, frameNames.Count - 1) then
    if frameNames.Objects[i] <> nil then
      result := (frameNames.Objects[i] as TFrameIndexInfo).EndFrame;
end;

//------------------------------------------------------------------------------

function TModel.StartFrame(const frame: string): integer;
begin
  result := StartFrame(frameNames.IndexOf(frame));
  if result = -1 then result := StartFrame(frameNames.IndexOf(UpperCase(frame)));
end;

//------------------------------------------------------------------------------

function TModel.EndFrame(const frame: string): integer;
begin
  result := EndFrame(frameNames.IndexOf(frame));
  if result = -1 then result := EndFrame(frameNames.IndexOf(UpperCase(frame)));
end;

//------------------------------------------------------------------------------

function TModel.FrameName(const i: integer): string;
begin
  if IsIntegerInRange(i, 0, frameNames.Count - 1) then
    result := frameNames.Strings[i]
  else
    result := EmptyStr;
end;

//------------------------------------------------------------------------------

function TModel.FrameIndex(const frame: string): integer;
begin
  result := frameNames.IndexOf(frame);
  if result = -1 then result := frameNames.IndexOf(UpperCase(frame));
end;

//------------------------------------------------------------------------------
//--------------------------- MD2 Model Class ----------------------------------
//------------------------------------------------------------------------------

constructor TModelMD2.Create(aName: string; aStream: TStream);
begin
  m_index_list := nil;
  m_frame_list := nil;
  fm_iTriangles := 0;
  fInternalListsValid := false;
  Inherited Create(aName, aStream);
end;

//------------------------------------------------------------------------------

destructor TModelMD2.Destroy;
begin
  DisposeLists;
  Inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TModelMD2.DisposeLists;
var i: integer;
begin
  if fInternalListsValid then
  begin
    if Assigned(m_frame_list) then
    begin
      for i := 0 to NumFrames - 1 do
        Dispose(m_frame_list[i].vertex);
      Dispose(m_frame_list);
      m_frame_list := nil;
    end;
    if Assigned(m_index_list) then
    begin
      Dispose(m_index_list);
      m_index_list := nil;
    end;
    fInternalListsValid := false;
  end;
end;

//------------------------------------------------------------------------------

function TModelMD2.LoadFromStream(aStream: TStream): boolean;
var
   g_skins: array[0..MD2_MAX_SKINS - 1, 0..63] of Char;
   base_st: array[0..MD2_MAX_VERTS - 1] of TMD2DstVert_T;
   buffer: array[0..MD2_MAX_VERTS * 4 + 128 - 1] of Byte;
   modelheader: TDmd2_T;
   tri: TMD2Triangle_T;
   out_t: PMD2AliasFrame_T;
   i, j: Integer;
   frameName: string;
begin
  DisposeLists;

  aStream.Read(ModelHeader, SizeOf(ModelHeader));

  if ModelHeader.ident <> MD2_MAGIC then
  begin
    result := false;
    exit;
  end;

  fNumFrames := modelheader.num_frames;
  fm_iTriangles := modelheader.num_tris;
  fNumVertexes := fm_iTriangles * 3;

  GetMem(m_index_list, SizeOf(TMD2_Index_List) * modelheader.num_tris);
  GetMem(m_frame_list, SizeOf(TMD2_Frame_List) * modelheader.num_frames);

  for i := 0 to modelheader.num_frames - 1 do
    GetMem(m_frame_list[i].vertex, SizeOf(TMD2_Vertex_List) * modelheader.num_xyz);

  aStream.Read(g_Skins, modelheader.num_skins * MD2_MAX_SKINNAME);
  aStream.Read(base_st, modelheader.num_st * SizeOf(base_st[0]));

  for i := 0 to modelheader.num_tris - 1 do
  begin
    aStream.Read(Tri, SizeOf(TMD2Triangle_T));
    with m_index_list[i] do
    begin
      a := tri.index_xyz[2];
      b := tri.index_xyz[1];
      c := tri.index_xyz[0];
      a_s := base_st[tri.index_st[2]].s / ModelHeader.skinwidth;
      a_t := base_st[tri.index_st[2]].t / ModelHeader.skinHeight;
      b_s := base_st[tri.index_st[1]].s / ModelHeader.skinwidth;
      b_t := base_st[tri.index_st[1]].t / ModelHeader.skinHeight;
      c_s := base_st[tri.index_st[0]].s / ModelHeader.skinwidth;
      c_t := base_st[tri.index_st[0]].t / ModelHeader.skinHeight;
    end;
  end;

  for i := 0 to modelheader.num_frames - 1 do
  begin
    out_t := PMD2AliasFrame_T(@buffer);
    aStream.Read(out_t^, modelheader.framesize);
    frameName := TrimStr(out_t^.name);
    if Copy(frameName, Length(frameName) - 1, 1)[1] in ['0'..'9'] then
      frameName := Copy(frameName, 1, Length(frameName) - 2)
    else frameName := Copy(frameName, 1, Length(frameName) - 1);
    if frameNames.IndexOf(frameName) < 0 then
    begin
      frameNames.AddObject(frameName, TFrameIndexInfo.Create);
      (frameNames.Objects[frameNames.Count - 1] as TFrameIndexInfo).StartFrame := i;
      (frameNames.Objects[frameNames.Count - 1] as TFrameIndexInfo).EndFrame := i;
    end
    else
      (frameNames.Objects[frameNames.IndexOf(frameName)] as TFrameIndexInfo).EndFrame := i;

    for j := 0 to modelheader.num_xyz - 1 do
    begin
      with m_frame_list[i].vertex[j] do
      begin
        x := out_t^.verts[j].v[0] * out_t^.scale[0] + out_t^.translate[0];
        y := out_t^.verts[j].v[1] * out_t^.scale[1] + out_t^.translate[1];
        z := out_t^.verts[j].v[2] * out_t^.scale[2] + out_t^.translate[2];
      end;
    end;
  end;
  result := true;
  fInternalListsValid := true;
end;

//------------------------------------------------------------------------------

procedure TModelMD2.BuildList;
begin
  Inherited BuildList;
  DisposeLists;
end;

//------------------------------------------------------------------------------

procedure TModelMD2.BuildFrame(frm: integer);
var i, j: integer;
begin
  if not IsIntegerInRange(frm, 0, fNumFrames - 1) then exit;

  if frm = 0 then
  begin
    i := 0;
    for j := 0 to fm_iTriangles - 1 do
    begin
      UV[i].u := m_index_list[j].a_s;
      UV[i].v := m_index_list[j].a_t;
      inc(i);

      UV[i].u := m_index_list[j].b_s;
      UV[i].v := m_index_list[j].b_t;
      inc(i);

      UV[i].u := m_index_list[j].c_s;
      UV[i].v := m_index_list[j].c_t;
      inc(i);
    end;
  end;

  i := 0;
  for j := 0 to fm_iTriangles - 1 do
  begin
    TheVectorsArray[frm][i] :=
      MakeD3DVector(
        m_frame_list[frm].vertex[m_index_list[j].a].y, // Switch to D3D Coordinates
        m_frame_list[frm].vertex[m_index_list[j].a].z,
        m_frame_list[frm].vertex[m_index_list[j].a].x);
    inc(i);

    TheVectorsArray[frm][i] :=
      MakeD3DVector(
        m_frame_list[frm].vertex[m_index_list[j].b].y,
        m_frame_list[frm].vertex[m_index_list[j].b].z,
        m_frame_list[frm].vertex[m_index_list[j].b].x);
    inc(i);

    TheVectorsArray[frm][i] :=
      MakeD3DVector(
        m_frame_list[frm].vertex[m_index_list[j].c].y,
        m_frame_list[frm].vertex[m_index_list[j].c].z,
        m_frame_list[frm].vertex[m_index_list[j].c].x);
    inc(i);
  end;
end;

//------------------------------------------------------------------------------

constructor TModelVRT.Create(aName: string; aStream : TStream);
begin
  fStream := aStream;
  Inherited Create(aName, aStream);
end;

//------------------------------------------------------------------------------

function TModelVRT.LoadFromStream(aStream: TStream): boolean;
var oldPos: integer;
begin
  result := true;
  fNumFrames := 1;

  frameNames.Clear;
  frameNames.AddObject(ExtractFileNameOnly(fName), TFrameIndexInfo.Create);
  (frameNames.Objects[0] as TFrameIndexInfo).StartFrame := 0;
  (frameNames.Objects[0] as TFrameIndexInfo).EndFrame := 0;
  oldPos := aStream.Position;
  aStream.Seek(SizeOf(TD3DCull), soFromBeginning);
  aStream.Read(fPrimitiveType, SizeOf(fPrimitiveType));

  fNumLVertexes := (aStream.Size - SizeOf(TD3DCull) - SizeOf(fPrimitiveType)) div SizeOf(TD3DLVertex);

  case fPrimitiveType of
    D3DPT_TRIANGLEFAN:
      fNumVertexes := Max(0, fNumLVertexes - 2 * 3);
    D3DPT_TRIANGLESTRIP:
      fNumVertexes := Max(0, 3 * (fNumLVertexes - 2));
    D3DPT_TRIANGLELIST:
      fNumVertexes := fNumLVertexes;
  else
    begin
      fNumFrames := 0;
      fNumVertexes := 0;
    end;
  end;

  aStream.Position := oldPos;
end;

//------------------------------------------------------------------------------

procedure TModelVRT.BuildFrame(frm: integer);
var i, oldPos: integer;
    VV: PD3DLVertexArray;
begin
  if frm = 0 then
  begin
    oldPos := fStream.Position;
    VV := nil;
    fStream.Seek(SizeOf(TD3DCull) + SizeOf(TD3DPrimitiveType), soFromBeginning);
    ReAllocMem(VV, fNumLVertexes * SizeOf(TD3DLVertex));
    fStream.Read(VV^, fNumLVertexes * SizeOf(TD3DLVertex));
    case fPrimitiveType of
      D3DPT_TRIANGLEFAN:
        for i := 1 to fNumLVertexes - 2 do
        begin
          TheVectorsArray[0][3 * (i - 1)].x := VV[0].x;
          TheVectorsArray[0][3 * (i - 1)].y := VV[0].y;
          TheVectorsArray[0][3 * (i - 1)].z := VV[0].z;
          UV[3*(i-1)].u := VV[0].tu;
          UV[3*(i-1)].v := VV[0].tv;
          TheVectorsArray[0][3 * (i - 1) + 1].x := VV[i].x;
          TheVectorsArray[0][3 * (i - 1) + 1].y := VV[i].y;
          TheVectorsArray[0][3 * (i - 1) + 1].z := VV[i].z;
          UV[3 * (i - 1) + 1].u := VV[i].tu;
          UV[3 * (i - 1) + 1].v := VV[i].tv;
          TheVectorsArray[0][3 * (i - 1) + 2].x := VV[i + 1].x;
          TheVectorsArray[0][3 * (i - 1) + 2].y := VV[i + 1].y;
          TheVectorsArray[0][3 * (i - 1) + 2].z := VV[i + 1].z;
          UV[3 * (i - 1) + 2].u := VV[i + 1].tu;
          UV[3 * (i - 1) + 2].v := VV[i + 1].tv;
        end;

      D3DPT_TRIANGLESTRIP:
        for i := 0 to fNumLVertexes - 3 do
            if odd(i) then
            begin
              TheVectorsArray[0][3 * i].x := VV[i].x;
              TheVectorsArray[0][3 * i].y := VV[i].y;
              TheVectorsArray[0][3 * i].z := VV[i].z;
              UV[3 * i].u := VV[i].tu;
              UV[3 * i].v := VV[i].tv;
              TheVectorsArray[0][3 * i + 1].x := VV[i + 2].x;
              TheVectorsArray[0][3 * i + 1].y := VV[i + 2].y;
              TheVectorsArray[0][3 * i + 1].z := VV[i + 2].z;
              UV[3 * i + 1].u := VV[i + 2].tu;
              UV[3 * i + 1].v := VV[i + 2].tv;
              TheVectorsArray[0][3 * i + 2].x := VV[i + 1].x;
              TheVectorsArray[0][3 * i + 2].y := VV[i + 1].y;
              TheVectorsArray[0][3 * i + 2].z := VV[i + 1].z;
              UV[3 * i + 2].u := VV[i + 1].tu;
              UV[3 * i + 2].v := VV[i + 1].tv;
            end
            else
            begin
              TheVectorsArray[0][3 * i].x := VV[i].x;
              TheVectorsArray[0][3 * i].y := VV[i].y;
              TheVectorsArray[0][3 * i].z := VV[i].z;
              UV[3 * i].u := VV[i].tu;
              UV[3 * i].v := VV[i].tv;
              TheVectorsArray[0][3 * i + 1].x := VV[i + 1].x;
              TheVectorsArray[0][3 * i + 1].y := VV[i + 1].y;
              TheVectorsArray[0][3 * i + 1].z := VV[i + 1].z;
              UV[3 * i + 1].u := VV[i + 2].tu;
              UV[3 * i + 1].v := VV[i + 2].tv;
              TheVectorsArray[0][3 * i + 2].x := VV[i + 2].x;
              TheVectorsArray[0][3 * i + 2].y := VV[i + 2].y;
              TheVectorsArray[0][3 * i + 2].z := VV[i + 2].z;
              UV[3 * i + 2].u := VV[i + 1].tu;
              UV[3 * i + 2].v := VV[i + 1].tv;
            end;
      D3DPT_TRIANGLELIST:
        for i := 0 to fNumVertexes - 1 do
        begin
          TheVectorsArray[0][i].x := VV[i].x;
          TheVectorsArray[0][i].y := VV[i].y;
          TheVectorsArray[0][i].z := VV[i].z;
          UV[i].u := VV[i].tu;
          UV[i].v := VV[i].tv;
        end;
    end;

    ReAllocMem(VV, 0);
    fStream.Position := oldPos;
  end;
end;

//------------------------------------------------------------------------------

constructor TModelVRM.Create(aName: string; aStream : TStream);
begin
  fStream := aStream;
  strmPos := 0;
  Inherited Create(aName, aStream);
end;

//------------------------------------------------------------------------------

function TModelVRM.LoadFromStream(aStream: TStream): boolean;
var s: string;
    i, j: integer;
    sf, ef: integer;
    len: integer;
    c: char;
    numAnims: integer;
begin
  s := '';
  for i := 1 to length(rsVrmMagic) do
  begin
    aStream.Read(c, SizeOf(c));
    s := s + c;
  end;

  result := s = rsVrmMagic;

  if not result then exit;

  aStream.Read(numAnims, SizeOf(numAnims));

  frameNames.Clear;

  fNumFrames := 0;

  if numAnims > 0 then
  begin
    for i := 0 to numAnims - 1 do
    begin
      aStream.Read(len, SizeOf(len));
      s := '';
      for j := 0 to len - 1 do
      begin
        aStream.Read(c, SizeOf(c));
        s := s + c;
      end;
      aStream.Read(sf, SizeOf(sf));
      aStream.Read(ef, SizeOf(ef));
      frameNames.AddObject(s, TFrameIndexInfo.Create);
      (frameNames.Objects[i] as TFrameIndexInfo).StartFrame := sf;
      (frameNames.Objects[i] as TFrameIndexInfo).EndFrame := ef;

      if ef > fNumFrames then fNumFrames := ef;
      if sf > fNumFrames then fNumFrames := sf;
    end;
    fNumFrames := fNumFrames + 1;
  end;

  fStream.Read(fNumVertexes, SizeOf(fNumVertexes));
  strmPos := fStream.Position;

end;

//------------------------------------------------------------------------------

procedure TModelVRM.BuildFrame(frm: integer);
var i: integer;
begin
  if frm = 0 then
  begin
    fStream.Position := strmPos;
    for i := 0 to fNumVertexes - 1 do
      fStream.Read(UV[i], SizeOf(TD3Duv));
  end;
  for i := 0 to fNumVertexes - 1 do
    fStream.Read(TheVectorsArray[frm][i], SizeOf(TD3DVector));
end;

//------------------------------------------------------------------------------

{$IFNDEF NO_MODELX}
constructor TModelX.Create(aName: string; aStream : TStream);
begin
  if aStream.InheritsFrom(TMemoryStream) then
  begin
    fStream := aStream;
    fNeedsStreamFree := false;
  end
  else if aStream.InheritsFrom(TFileStream) then
  begin
    fStream := aStream;
    fNeedsStreamFree := false;
  end
  else
  begin
    fStream := TmemoryStream.Create;
    fStream.CopyFrom(aStream, 0);
    fNeedsStreamFree := true;
  end;

  Inherited Create(aName, aStream);
end;

//------------------------------------------------------------------------------

function TModelX.LoadFromStream(aStream: TStream): boolean;
begin
  result := true;
  fNumFrames := 1;

  frameNames.Clear;
  frameNames.AddObject(ExtractFileNameOnly(fName), TFrameIndexInfo.Create);
  (frameNames.Objects[0] as TFrameIndexInfo).StartFrame := 0;
  (frameNames.Objects[0] as TFrameIndexInfo).EndFrame := 0;
end;

//------------------------------------------------------------------------------

resourceString
  rsTmpX = '~DXMODEL.X';

function GetTmpXFileName: string;
var tempDir: string;
    l: integer;
begin
  SetLength(tempDir, MAX_PATH + 1);
  l := GetTempPath(MAX_PATH, PChar(tempDir));
  SetLength(tempDir, l);
  if Copy(tempDir,Length(tempDir), 1) <> '\' then tempDir := tempDir + '\';
  result := tempDir + rsTmpX;
end;

procedure TModelX.BuildFrame(frm: integer);
var
  MeshBuilder: IDirect3DRMMeshBuilder3;
  Mesh: IDirect3DRMMesh;
  RM: IDirect3DRM3;
  Frame: IDirect3DRMFrame3;
  i, j, k: integer;
  numGroups: integer;
  rmV: TD3DRMVertex;
  VV: PD3DRMVertexArray;
  vCount, fCount, vPerFace, fDataSize: Cardinal;
  ret: HResult;
  Groups: PIntegerArray;
  IArray: PIntegerArray;
  IICount: integer;
  fanIndex: integer;
  IIndex: Cardinal;
  bGetFaceCount: boolean;
  oldPos: integer;
  f: TFileStream;
  tmpFName: string;

{ Required changes to DirectX.pas:

//    function GetGroup(id: TD3DRMGroupIndex; var vCount, fCount, vPerFace,
//        fDataSize, fData: DWORD): HResult; stdcall; -> Changed to
    function GetGroup(id: TD3DRMGroupIndex; var vCount, fCount, vPerFace,
        fDataSize: DWORD; var fData): HResult; stdcall;
//    function GetVertices(id: TD3DRMGroupIndex; index: DWORD; count: DWORD;
//        var returnPtr: TD3DRMVertex): HResult; stdcall; -> Changed to
    function GetVertices(id: TD3DRMGroupIndex; index: DWORD; count: DWORD;
        returnPtr: PD3DRMVertex): HResult; stdcall;

}
  procedure AddIndex(index: integer);
  begin
    IArray[IICount] := index;
    inc(IICount);
  end;

begin
  if frm <> 0 then
    Exit;

  RM := GetD3DRM;
  RM.CreateMeshBuilder(MeshBuilder);
  oldPos := fStream.Position;
  fStream.Position := 0;
  if fStream.InheritsFrom(TFileStream) then
    ret := MeshBuilder.Load(PChar(fName), nil, D3DRMLOAD_FROMFILE, nil, nil)
  else
  begin
    tmpFName := GetTmpXFileName;
    f := TFileStream.Create(tmpFName, fmCreate or fmOpenReadWrite);
    try
      f.CopyFrom(fStream, 0);
    finally
      f.Free;
    end;
    ret := MeshBuilder.Load(PChar(tmpFName), nil, D3DRMLOAD_FROMFILE, nil, nil);
    DeleteFile(tmpFName);
  end;

  if ret <> D3DRM_OK then
  begin
    fStream.Position := 0;
    RM.CreateFrame(nil, Frame);
    if fStream.InheritsFrom(TFileStream) then
      Frame.Load(PChar(fName), nil, D3DRMLOAD_FROMFILE, nil, nil)
    else
    begin
      tmpFName := GetTmpXFileName;
      f := TFileStream.Create(tmpFName, fmCreate or fmOpenReadWrite);
      try
        f.CopyFrom(fStream, 0);
      finally
        f.Free;
      end;
      Frame.Load(PChar(tmpFName), nil, D3DRMLOAD_FROMFILE, nil, nil);
      DeleteFile(tmpFName);
    end;
    MeshBuilder.AddFrame(Frame);
  end;
  fStream.Position := oldPos;

  MeshBuilder.CreateMesh(Mesh);

  numGroups := Mesh.GetGroupCount;

  Groups := AllocMem($FFFF * SizeOf(Integer));
  IArray := AllocMem($FFFF * SizeOf(Integer));
  VV := AllocMem($FFFF * SizeOf(TD3DRMVertex));

  fNumVertexes := $FFFF;
  ReAllocMem(TheVectorsArray[0], fNumVertexes * SizeOf(TD3DVector));
  ReAllocMem(UV, fNumVertexes * SizeOf(TD3Duv));
  fNumVertexes := 0;

  for i := 0 to numGroups - 1 do
  begin
    Mesh.GetGroup(i, vCount, fCount, vPerFace,
        fDataSize, Groups^);

    IICount := 0;
    IIndex := 0;
    bGetFaceCount := vPerFace = 0;

    while IIndex < fDataSize do
    begin
      if bGetFaceCount then
      begin
        vPerFace := Groups[IIndex];
        inc(IIndex);
      end;

      fanIndex := IIndex;

      for j := 0 to vPerFace - 3 do
      begin
        // Add the fan point
        AddIndex(Groups[fanIndex]);
        // Add the 2 other points
        AddIndex(Groups[fanIndex + j + 1]);
        AddIndex(Groups[fanIndex + j + 2]);
      end;
      IIndex := IIndex + vPerFace;
    end;

    j := 0;
    while Mesh.GetVertices(i, j, 1, @rmV) = D3DRM_OK do
    begin
      VV[j] := rmV;
      inc(j);
    end;

    if IICount = 0 then
    begin
      for k := 0 to j - 3 do
      begin
        AddIndex(0);
        AddIndex(k + 1);
        AddIndex(k + 2);
      end;
    end;
{      if j > 2 then
      begin
        IICount := 3 * (j - 2);
        for k := 0 to j - 3 do
        begin
          if odd(k) then
          begin
            IArray[3 * k] := k;
            IArray[3 * k + 1] := k + 2;
            IArray[3 * k + 2] := k + 1;
          end
          else
          begin
            IArray[3 * k] := k;
            IArray[3 * k + 1] := k + 1;
            IArray[3 * k + 2] := k + 2;
          end;
        end;
      end;}

    for k := 0 to IICount - 1 do
    begin
      TheVectorsArray[0][fNumVertexes].x := VV[IArray[k]].position.x;
      TheVectorsArray[0][fNumVertexes].y := VV[IArray[k]].position.y;
      TheVectorsArray[0][fNumVertexes].z := VV[IArray[k]].position.z;
      UV[fNumVertexes].u := VV[IArray[k]].tu;
      UV[fNumVertexes].v := VV[IArray[k]].tv;
      inc(fNumVertexes);
    end;
  end;
//  ReAllocMem(rmVV, 0);
  ReAllocMem(TheVectorsArray[0], fNumVertexes * SizeOf(TD3DVector));
  ReAllocMem(UV, fNumVertexes * SizeOf(TD3Duv));
  ReAllocMem(Groups, 0);
  ReAllocMem(IArray, 0);
  ReAllocMem(VV, 0);

  DoCleanUp;
end;

//------------------------------------------------------------------------------

procedure TModelX.DoCleanUp;
begin
  if fNeedsStreamFree then
  begin
    fNeedsStreamFree := false;
    fStream.Free;
    fStream := nil;
  end;
end;

destructor TModelX.Destroy;
begin
  DoCleanUp;
  Inherited Destroy;
end;
{$ENDIF}

resourceString
  rsErrModelLoading = 'Error loading model.';
  rsErrModelInvalidFrame = 'Error accessing model frame.';

function ModelErrorToString(err: integer): string;
begin
  case err of
    ERR_MODEL_LOADING: result := rsErrModelLoading;
    ERR_MODEL_INVALIDFRAME: result := rsErrModelInvalidFrame;
  else
    result := '';
  end;
end;

initialization
  D3DRM3 := nil;

finalization

end.
