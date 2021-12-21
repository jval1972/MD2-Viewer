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
//  Designer specific utilities
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

{$I defs.inc}

unit se_DXDsngUtils;
{ Designer Specific utilities}

interface

{$IFDEF DESIGNER}
uses Windows, SysUtils, Classes, Graphics, Forms, Dialogs, Buttons, zLib,
  BinaryData, StdCtrls,
  zBitmap, xWZ, JPEG, JpgImg, Bmp2Tiff, xGIF, xPPM, DIB, BUPcx, xM8, xTGA,
  RAHLEditor;

type
  TImageFileType = (tiGeneric, tiBMZ, tiWZ1, tiWZ2, tiBMP, tiPPM, tiJPG, tiTif, tiPCX, tiM8, tiTGA);

  TImageFilter3x3 = record
    RAY: array[0..8] of integer;
    DIVFACTOR: word;
    BIAS: byte;
  end;

  TImageFilter5x5 = record
    RAY: array[0..24] of integer;
    DIVFACTOR: word;
    BIAS: byte;
  end;

procedure SaveImageAs(Graphic: TGraphic; FileType: TImageFileType; FileName: string);

function CopyFile(const FFrom, FTo: TFileName; doBackup: boolean = true): boolean;

function ConvertBitmapTo8bpp(bmp: TBitmap): boolean;

procedure ForceBitmapTo8bpp(bmp: TBitmap);

procedure ApplyFilter3x3ToBitmap(const flt: TImageFilter3x3; bmp: TBitmap);

procedure ApplyFilter5x5ToBitmap(const flt: TImageFilter5x5; bmp: TBitmap);

resourceString
  rsExtBMZ = '.bmz';
  rsExtWZ1 = '.wz1';
  rsExtWZ2 = '.wz2';
  rsExtBMP = '.bmp';
  rsExtPPM = '.ppm';
  rsExtJPG = '.jpg';
  rsExtTIF = '.tif';
  rsExtPCX = '.pcx';
  rsExtM8  = '.m8';
  rsExtTGA = '.tga';

procedure MakeCommonDialogButtonsFlat(AOwner: TComponent; flat: boolean);

function InvertBitmap(Bmp: TBitmap): boolean; overload;

function InvertBitmap(OrigBmp, DestBmp: TBitmap): boolean; overload;

procedure CopySymbolColor(source, dest: TSymbolColor);

procedure CopyHLColorSettings(source, dest: TRAHLEditor);

// Combobox helpers
procedure FlashComboBox(c: TComboBox);

function GetComboBoxIntValue(c: TComboBox): integer;

function GetComboBoxStrValue(c: TComboBox): string;

function SetComboBoxIntValue(c: TComboBox; Value: integer): boolean;

{$ENDIF}

implementation

{$IFDEF DESIGNER}
uses DXDUtils;

procedure MakeCommonDialogButtonsFlat(AOwner: TComponent; flat: boolean);
var i, j: integer;
begin
  for j := 0 to AOwner.ComponentCount - 1 do
  begin
    if AOwner.Components[j].InheritsFrom(TCommonDialog) then
      for i := 0 to AOwner.Components[j].ComponentCount - 1 do
        if (AOwner.Components[j].Components[i] is TSpeedbutton) then
          (AOwner.Components[j].Components[i] as TSpeedbutton).Flat := flat;
  end;
end;

procedure SaveImageAs(Graphic: TGraphic; FileType: TImageFileType; FileName: string);
var
  ext: string;
  aBitmap: TBitmap;
begin
  ext := ExtractFileExt(Filename);
  aBitmap := nil;
  if ext = '' then
  begin
    case FileType of
      tiBMZ: ext := rsExtBMZ;
      tiWZ1: ext := rsExtWZ1;
      tiWZ2: ext := rsExtWZ2;
      tiBMP: ext := rsExtBMP;
      tiPPM: ext := rsExtPPM;
      tiJPG: ext := rsExtJPG; // <-----------------
      tiTif: ext := rsExtTif;
      tiPCX: ext := rsExtPCX;
      tiM8 : ext := rsExtM8;
      tiTGA: ext := rsExtTGA;
    else
    end;
    Filename := Filename + ext;
  end;
  if (UpperCase(ext) = UpperCase(rsExtBMP)) or // <-----------------
     (UpperCase(ext) = UpperCase(rsExtJPG)) or
     (UpperCase(ext) = UpperCase(rsExtPCX)) or
     (UpperCase(ext) = UpperCase(rsExtTIF)) then aBitmap := TBitmap.Create
  else if UpperCase(ext) = UpperCase(rsExtWZ1) then aBitmap := TWZ1Bitmap.Create
  else if UpperCase(ext) = UpperCase(rsExtWZ2) then aBitmap := TWZ2Bitmap.Create
  else if UpperCase(ext) = UpperCase(rsExtM8) then aBitmap := TM8Bitmap.Create
  else if UpperCase(ext) = UpperCase(rsExtTGA) then aBitmap := TTGABitmap.Create
  else if UpperCase(ext) = UpperCase(rsExtBMZ) then
  begin
    aBitmap := TZBitmap.Create;
    TZBitmap(aBitmap).CompressionLevel := clMax;
  end
  else if UpperCase(ext) = UpperCase(rsExtPPM) then aBitmap := TPPMBitmap.Create;
  if Assigned(aBitmap) then
  try
    CreateBackupFile(Filename);
    aBitmap.Width := Graphic.Width;
    aBitmap.Height := Graphic.Height;
    aBitmap.Canvas.Draw(0, 0, Graphic);

    if UpperCase(ext) = UpperCase(rsExtPCX) then
    begin
      with TBUPcx.Create do
      begin
        Assign(aBitmap);
        SaveToFile(FileName);
      end
    end
    else if UpperCase(ext) = UpperCase(rsExtJpg) then
      with TJPEGImage.Create(Application.MainForm) do
      begin // <-----------------
        Visible := false;
        try
          Picture.Bitmap.Width := aBitmap.Width;
          Picture.Bitmap.Height := aBitmap.Height;
          Picture.Bitmap.PixelFormat := pf24bit;
          Picture.Bitmap.Canvas.Draw(0, 0, aBitmap);
          SaveToFile(Filename);
        finally
          Free;
        end;
      end
    else if UpperCase(ext) = UpperCase(rsExtTIF) then
    begin
      ConvertBitmapTo8bpp(aBitmap);
      WriteTiffToFile(Filename, aBitmap);
    end
    else
    begin
      if (UpperCase(ext) <> UpperCase(rsExtWZ1)) and
         (UpperCase(ext) <> UpperCase(rsExtWZ2)) then
        ConvertBitmapTo8bpp(aBitmap);
      aBitmap.SaveToFile(Filename);
    end;
  finally
    aBitmap.Free;
  end
end;

function CopyFile(const FFrom, FTo: TFileName; doBackup: boolean = true): boolean;
var f1, f2: TFileStream;
begin
  result := FileExists(FFrom);
  if result then
  begin
    f1 := TFileStream.Create(FFrom, fmOpenRead or fmShareDenyWrite);
    if doBackup then CreateBackupFile(FTo);
    f2 := TFileStream.Create(FTo, fmCreate or fmShareDenyWrite);
    try
      f2.CopyFrom(f1, 0);
    finally
      f1.Free;
      f2.Free;
    end;
  end;
end;

type
  TColorAppearence = record
    color: TColor;
    num: integer;
  end;

  TColorAppearences = array[0..$FFFF] of TColorAppearence;
  PColorAppearences = ^TColorAppearences;

procedure ExChangeBytes(var b1, b2: byte);
var b: byte;
begin
  b := b1;
  b1 := b2;
  b2 := b;
end;

procedure ExChangePalEntries(var c1, c2: TPaletteEntry);
var c: TPaletteEntry;
begin
  c := c1;
  c1 := c2;
  c2 := c;
end;

procedure MakeThePaletteWindowsStyle(lpal: pLogPalette; BMPData: PByteArray; numPixels: integer);
var i: integer;
begin
  for i := 8 to 15 do
    ExChangePalEntries(lPal.palPalEntry[i], lPal.palPalEntry[240 + i]);
  for i := 0 to numPixels - 1 do
  begin
    if BMPData[i] in [8..15] then
      BMPData[i] := BMPData[i] + 240
    else if BMPData[i] in [248..255] then
      BMPData[i] := BMPData[i] - 240
  end;
end;

function ConvertBitmapTo8bpp(bmp: TBitmap): boolean;
// Μετατρέπει ένα bitmap σε 8 bits per pixel, επιστρέφει false αν το
// bitmap έχει παραπάνω από 256 διαφορετικά χρώματα και δεν κάνει την μετατροπή.
var CC: PColorAppearences;
    i, j, k: integer;
    b: PByteArray;
    numC: integer;
    c: TColor;
    found: boolean;
    lpal: PLogPalette;
    hpal: HPALETTE;
    newBMPData: PByteArray;
begin
  result := true;
  if bmp.PixelFormat in [pf1bit, pf4bit] then
    bmp.PixelFormat := pf8bit
  else if bmp.PixelFormat <> pf8bit then
  begin
    bmp.PixelFormat := pf24bit;
    numC := 16;
    CC := nil;
    ReAllocMem(CC, 16 * SizeOf(TColorAppearence));
    // Default Windows Palette
    CC[0].color := RGB(0, 0, 0);
    CC[0].num := 0;
    CC[1].color := RGB(128, 0, 0);
    CC[1].num := 0;
    CC[2].color := RGB(0, 128, 0);
    CC[2].num := 0;
    CC[3].color := RGB(128, 128, 0);
    CC[3].num := 0;
    CC[4].color := RGB(0, 0, 128);
    CC[4].num := 0;
    CC[5].color := RGB(128, 0, 128);
    CC[5].num := 0;
    CC[6].color := RGB(0, 128, 128);
    CC[6].num := 0;
    CC[7].color := RGB(128, 128, 128);
    CC[7].num := 0;
    CC[8].color := RGB(192, 192, 192);
    CC[8].num := 0;
    CC[9].color := RGB(255, 0, 0);
    CC[9].num := 0;
    CC[10].color := RGB(0, 255, 0);
    CC[10].num := 0;
    CC[11].color := RGB(255, 255, 0);
    CC[11].num := 0;
    CC[12].color := RGB(0, 0, 255);
    CC[12].num := 0;
    CC[13].color := RGB(255, 0, 255);
    CC[13].num := 0;
    CC[14].color := RGB(0, 255, 255);
    CC[14].num := 0;
    CC[15].color := RGB(255, 255, 255);
    CC[15].num := 0;

    GetMem(newBMPData, bmp.Width * bmp.Height);
    for i := 0 to bmp.Height - 1 do
    begin
      b := bmp.ScanLine[i];
      for j := 0 to bmp.Width - 1 do
      begin
        c := RGB(b[3 * j + 2], b[3 * j + 1], b[3 * j]);
        found := false;
        for k := 0 to numC - 1 do
        begin
        // Το χρώμα υπάρχει ήδη στην παλέττα, αυξάνουμε τον αριθμό εμφανίσεων
          if CC[k].color = c then
          begin
            found := true;
            CC[k].num := CC[k].num + 1;
            newBMPData[i * bmp.Width + j] := k;
            break;
          end;
        end;
        // Το χρώμα δεν υπάρχει στην παλέττα, προσθέτουμε το νέο χρώμα
        if not found then
        begin
          inc(numC);
          if numC > 255 then break;
          ReAllocMem(CC, numC * SizeOf(TColorAppearence));
          newBMPData[i * bmp.Width + j] := numC - 1;
          CC[numC - 1].color := c;
          CC[numC - 1].num := 1;
        end;
      end;
      if numC > 255 then break;
    end;
    if numC > 255 then // περισσότερα από 256 χρώματα, δεν αλλάζουμε την εικόνα
      result := false
    else
    begin
      GetMem(lpal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 255);
      lpal.palVersion := $300;
      lpal.palNumEntries := 256;
      for i := 0 to numC - 1 do
      begin
        lpal.palPalEntry[i].peRed := GetRValue(CC[i].color);
        lpal.palPalEntry[i].peGreen := GetGValue(CC[i].color);
        lpal.palPalEntry[i].peBlue := GetBValue(CC[i].color);
      end;
      for i := numC to 255 do
      begin
        lpal.palPalEntry[i].peRed := 0;
        lpal.palPalEntry[i].peGreen := 0;
        lpal.palPalEntry[i].peBlue := 0;
      end;
      MakeThePaletteWindowsStyle(lpal, newBMPData, bmp.Width * bmp.Height);
      bmp.PixelFormat := pf8bit;
      hpal := CreatePalette(lpal^);
      if hpal <> 0 then bmp.Palette := hpal;
      for i := 0 to bmp.Height - 1 do
      begin
        b := bmp.ScanLine[i];
        for j := 0 to bmp.Width - 1 do
          b[j] := newBMPData[i * bmp.Width + j];
      end;
      FreeMem(lpal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 255);
//      DeleteObject(hpal);
    end;
    FreeMem(newBMPData, bmp.Width * bmp.Height);
    ReAllocMem(CC, 0);
  end;
end;

function AlmostEqualColors(c1,c2: TColor): boolean;
begin
  result := sqr(integer(GetRValue(c1)) - integer(GetRValue(c2))) +
            sqr(integer(GetGValue(c1)) - integer(GetGValue(c2))) +
            sqr(integer(GetBValue(c1)) - integer(GetBValue(c2))) <= 256;
end;

procedure ForceBitmapTo8bpp(bmp: TBitmap);
// Μετατρέπει ένα bitmap σε 8 bits per pixel, επιστρέφει false αν το
// bitmap έχει παραπάνω από 256 διαφορετικά χρώματα κάνει προσέγγιση
var CC: PColorAppearences;
    i, j, k: integer;
    b: PByteArray;
    numC: integer;
    c: TColor;
    found: boolean;
    lpal: PLogPalette;
    hpal: HPALETTE;
    newBMPData: PByteArray;
    dist, dist1: integer;
    index: integer;
begin
  if bmp.PixelFormat in [pf1bit, pf4bit] then
    bmp.PixelFormat := pf8bit
  else if bmp.PixelFormat <> pf8bit then
  begin
    bmp.PixelFormat := pf24bit;
    numC := 16;
    CC := nil;
    ReAllocMem(CC, 16 * SizeOf(TColorAppearence));
    // Default Windows Palette
    CC[0].color := RGB(0, 0, 0);
    CC[0].num := 0;
    CC[1].color := RGB(128, 0, 0);
    CC[1].num := 0;
    CC[2].color := RGB(0, 128, 0);
    CC[2].num := 0;
    CC[3].color := RGB(128, 128, 0);
    CC[3].num := 0;
    CC[4].color := RGB(0, 0, 128);
    CC[4].num := 0;
    CC[5].color := RGB(128, 0, 128);
    CC[5].num := 0;
    CC[6].color := RGB(0, 128, 128);
    CC[6].num := 0;
    CC[7].color := RGB(128, 128, 128);
    CC[7].num := 0;
    CC[8].color := RGB(192, 192, 192);
    CC[8].num := 0;
    CC[9].color := RGB(255, 0, 0);
    CC[9].num := 0;
    CC[10].color := RGB(0, 255, 0);
    CC[10].num := 0;
    CC[11].color := RGB(255, 255, 0);
    CC[11].num := 0;
    CC[12].color := RGB(0, 0, 255);
    CC[12].num := 0;
    CC[13].color := RGB(255, 0, 255);
    CC[13].num := 0;
    CC[14].color := RGB(0, 255, 255);
    CC[14].num := 0;
    CC[15].color := RGB(255, 255, 255);
    CC[15].num := 0;

    GetMem(newBMPData, bmp.Width * bmp.Height);
    for i := 0 to bmp.Height - 1 do
    begin
      b := bmp.ScanLine[i];
      for j := 0 to bmp.Width - 1 do
      begin
        c := RGB(b[3 * j + 2], b[3 * j + 1], b[3 * j]);
        found := false;
        for k := 0 to numC - 1 do
        begin
        // Το χρώμα υπάρχει ήδη στην παλέττα, αυξάνουμε τον αριθμό εμφανίσεων
          if AlmostEqualColors(CC[k].color, c) then
//          if CC[k].color = c then
          begin
            found := true;
            CC[k].num := CC[k].num + 1;
            newBMPData[i * bmp.Width + j] := k;
            break;
          end;
        end;
        // Το χρώμα δεν υπάρχει στην παλέττα, προσθέτουμε το νέο χρώμα
        if not found then
        begin
          if numC < 256 then  // Αν έχουμε λιγότερα από 256 χρώματα
          begin
            inc(numC);
            ReAllocMem(CC, numC * SizeOf(TColorAppearence));
            newBMPData[i * bmp.Width + j] := numC - 1;
            CC[numC - 1].color := c;
            CC[numC - 1].num := 1;
          end
          else
          begin
          // Βρίσκουμε το κοντινότερο χρώμα (προσέγγιση)
            dist := MAXINT;
            index := 0;
            for k := 0 to 255 do
            begin
              dist1 := abs(integer(GetRValue(CC[k].color)) - integer(GetRValue(c))) *
                       abs(integer(GetGValue(CC[k].color)) - integer(GetGValue(c))) *
                       abs(integer(GetBValue(CC[k].color)) - integer(GetBValue(c)));
              if dist1 < dist then
              begin
                dist := dist1;
                index := k;
              end;
            end;
            newBMPData[i * bmp.Width + j] := index;
          end;
        end;
      end;
    end;
    GetMem(lpal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 255);
    lpal.palVersion := $300;
    lpal.palNumEntries := 256;
    for i := 0 to numC - 1 do
    begin
      lpal.palPalEntry[i].peRed := GetRValue(CC[i].color);
      lpal.palPalEntry[i].peGreen := GetGValue(CC[i].color);
      lpal.palPalEntry[i].peBlue := GetBValue(CC[i].color);
    end;
    for i := numC to 255 do
    begin
      lpal.palPalEntry[i].peRed := 0;
      lpal.palPalEntry[i].peGreen := 0;
      lpal.palPalEntry[i].peBlue := 0;
    end;
    MakeThePaletteWindowsStyle(lpal, newBMPData, bmp.Width * bmp.Height);
    bmp.PixelFormat := pf8bit;
    hpal := CreatePalette(lpal^);
    if hpal <> 0 then bmp.Palette := hpal;
    for i := 0 to bmp.Height - 1 do
    begin
      b := bmp.ScanLine[i];
      for j := 0 to bmp.Width - 1 do
        b[j] := newBMPData[i * bmp.Width + j];
    end;
    FreeMem(lpal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 255);
//    DeleteObject(hpal);

    FreeMem(newBMPData, bmp.Width * bmp.Height);
    ReAllocMem(CC, 0);
  end;
end;

type    // For scanline simplification
  TRGBArray = array[0..32767] OF TRGBTriple;
  PRGBArray = ^TRGBArray;

{This just forces a value to be 0 - 255 for rgb purposes.  I used asm in an
 attempt at speed, but I don't think it helps much.}
function Set255(Clr : integer): integer;
asm
  MOV  EAX,Clr  // store value in EAX register (32-bit register)
  CMP  EAX,254  // compare it to 254
  JG   @SETHI   // if greater than 254 then go set to 255 (max value)
  CMP  EAX,1    // if less than 255, compare to 1
  JL   @SETLO   // if less than 1 go set to 0 (min value)
  RET           // otherwise it doesn't change, just exit
@SETHI:         // Set value to 255
  MOV  EAX,255  // Move 255 into the EAX register
  RET           // Exit (result value is the EAX register value)
@SETLO:         // Set value to 0
  MOV  EAX,0    // Move 0 into EAX register
end;            // Result is in EAX

procedure ApplyFilter3x3ToBitmap(const flt: TImageFilter3x3; bmp: TBitmap);
var
  O, T, C, B: PRGBArray;  // Scanlines
  x, y: integer;
  tBufr: TBitmap; // temp bitmap for 'enlarged' image
begin
  tBufr := TBitmap.Create;
  try
    tBufr.Width := bmp.Width + 2;  // Add a box around the outside...
    tBufr.Height := bmp.Height + 2;
    tBufr.PixelFormat := pf24bit;
    bmp.PixelFormat := pf24bit;
    O := tBufr.ScanLine[0];   // Copy top corner pixels
    T := bmp.ScanLine[0];
    O[0] := T[0];  // Left
    O[tBufr.Width - 1] := T[bmp.Width - 1];  // Right
    // Copy bottom line to our top - trying to remain seamless...
    tBufr.Canvas.CopyRect(Rect(1, 0, tBufr.Width - 1, 1), bmp.Canvas,
                          Rect(0, bmp.Height - 1, bmp.Width, bmp.Height - 2));

    O := tBufr.ScanLine[tBufr.Height - 1]; // Copy bottom corner pixels
    T := bmp.ScanLine[bmp.Height - 1];
    O[0] := T[0];
    O[tBufr.Width - 1] := T[bmp.Width - 1];
    // Copy top line to our bottom
    tBufr.Canvas.CopyRect(Rect(1, tBufr.Height - 1, tBufr.Width - 1, tBufr.Height), bmp.Canvas,
                          Rect(0, 0, bmp.Width, 1));
    // Copy left to our right
    tBufr.Canvas.CopyRect(Rect(tBufr.Width - 1, 1, tBufr.Width, tBufr.Height - 1), bmp.Canvas,
                          Rect(0, 0, 1, bmp.Height));
    // Copy right to our left
    tBufr.Canvas.CopyRect(Rect(0, 1, 1, tBufr.Height - 1), bmp.Canvas,
                          Rect(bmp.Width - 1, 0, bmp.Width, bmp.Height));
    // Now copy main rectangle
    tBufr.Canvas.CopyRect(Rect(1, 1, tBufr.Width - 1, tBufr.Height - 1), bmp.Canvas,
                          Rect(0, 0, bmp.Width, bmp.Height));
    // bmp now enlarged and copied, apply convolve
    for x := 0 to bmp.Height - 1 do
    begin  // Walk scanlines
      O := bmp.ScanLine[x];       // New Target (Original)
      T := tBufr.ScanLine[x];     //old x-1  (Top)
      C := tBufr.ScanLine[x + 1]; //old x    (Center)
      B := tBufr.ScanLine[x + 2]; //old x+1  (Bottom)
    // Now do the main piece
      for y := 1 to (tBufr.Width - 2) do
      begin  // Walk pixels
        O[y - 1].rgbtRed := Set255(
            ((T[y - 1].rgbtRed * flt.RAY[0]) +
            (T[y].rgbtRed * flt.RAY[1]) + (T[y + 1].rgbtRed * flt.RAY[2]) +
            (C[y - 1].rgbtRed * flt.RAY[3]) +
            (C[y].rgbtRed * flt.RAY[4]) + (C[y + 1].rgbtRed * flt.RAY[5])+
            (B[y - 1].rgbtRed * flt.RAY[6]) +
            (B[y].rgbtRed * flt.RAY[7]) + (B[y + 1].rgbtRed * flt.RAY[8])) div flt.DIVFACTOR + flt.BIAS
            );
        O[y - 1].rgbtBlue := Set255(
            ((T[y - 1].rgbtBlue * flt.RAY[0]) +
            (T[y].rgbtBlue * flt.RAY[1]) + (T[y + 1].rgbtBlue * flt.RAY[2]) +
            (C[y - 1].rgbtBlue * flt.RAY[3]) +
            (C[y].rgbtBlue * flt.RAY[4]) + (C[y + 1].rgbtBlue * flt.RAY[5])+
            (B[y - 1].rgbtBlue * flt.RAY[6]) +
            (B[y].rgbtBlue * flt.RAY[7]) + (B[y + 1].rgbtBlue * flt.RAY[8])) div flt.DIVFACTOR + flt.BIAS
            );
        O[y - 1].rgbtGreen := Set255(
            ((T[y - 1].rgbtGreen * flt.RAY[0]) +
            (T[y].rgbtGreen * flt.RAY[1]) + (T[y + 1].rgbtGreen * flt.RAY[2]) +
            (C[y - 1].rgbtGreen * flt.RAY[3]) +
            (C[y].rgbtGreen * flt.RAY[4]) + (C[y + 1].rgbtGreen * flt.RAY[5])+
            (B[y - 1].rgbtGreen * flt.RAY[6]) +
            (B[y].rgbtGreen * flt.RAY[7]) + (B[y + 1].rgbtGreen * flt.RAY[8])) div flt.DIVFACTOR + flt.BIAS
            );
      end;
    end;
  finally
    tBufr.Free;
  end;
end;

procedure ApplyFilter5x5ToBitmap(const flt: TImageFilter5x5; bmp: TBitmap);
var
  O, T, TT, C, B, BB: PRGBArray;  // Scanlines
  x, y: integer;
  tBufr: TBitmap; // temp bitmap for 'enlarged' image
begin
  tBufr := TBitmap.Create;
  try
    tBufr.Width := bmp.Width + 4;  // Add a box around the outside...
    tBufr.Height := bmp.Height + 4;
    tBufr.PixelFormat := pf24bit;
    bmp.PixelFormat := pf24bit;
    O := tBufr.ScanLine[0];   // Copy top corner pixels
    T := bmp.ScanLine[0];
    O[0] := T[0];  // Left
    O[1] := T[1];
    O[tBufr.Width - 2] := T[bmp.Width - 2];  // Right
    O[tBufr.Width - 1] := T[bmp.Width - 1];  // Right
    O := tBufr.ScanLine[1];   // Copy top corner pixels
    T := bmp.ScanLine[1];
    O[0] := T[0];  // Left
    O[1] := T[1];
    O[tBufr.Width - 2] := T[bmp.Width - 2];  // Right
    O[tBufr.Width - 1] := T[bmp.Width - 1];  // Right
    // Copy bottom line to our top - trying to remain seamless...
    tBufr.Canvas.CopyRect(Rect(2, 0, tBufr.Width - 2, 2), bmp.Canvas,
                          Rect(0, bmp.Height - 2, bmp.Width, bmp.Height - 4));

    O := tBufr.ScanLine[tBufr.Height - 2]; // Copy bottom corner pixels
    T := bmp.ScanLine[bmp.Height - 2];
    O[0] := T[0];
    O[1] := T[1];
    O[tBufr.Width - 2] := T[bmp.Width - 2];
    O[tBufr.Width - 1] := T[bmp.Width - 1];
    O := tBufr.ScanLine[tBufr.Height - 1]; // Copy bottom corner pixels
    T := bmp.ScanLine[bmp.Height - 1];
    O[0] := T[0];
    O[1] := T[1];
    O[tBufr.Width - 2] := T[bmp.Width - 2];
    O[tBufr.Width - 1] := T[bmp.Width - 1];
    // Copy top line to our bottom
    tBufr.Canvas.CopyRect(Rect(2, tBufr.Height - 2, tBufr.Width - 2, tBufr.Height), bmp.Canvas,
                          Rect(0, 0, bmp.Width, 2));
    // Copy left to our right
    tBufr.Canvas.CopyRect(Rect(tBufr.Width - 2, 2, tBufr.Width, tBufr.Height - 2), bmp.Canvas,
                          Rect(0, 0, 2, bmp.Height));
    // Copy right to our left
    tBufr.Canvas.CopyRect(Rect(0, 2, 2, tBufr.Height - 2), bmp.Canvas,
                          Rect(bmp.Width - 2, 0, bmp.Width, bmp.Height));
    // Now copy main rectangle
    tBufr.Canvas.CopyRect(Rect(2, 2, tBufr.Width - 2, tBufr.Height - 2), bmp.Canvas,
                          Rect(0, 0, bmp.Width, bmp.Height));
    // bmp now enlarged and copied, apply convolve
    for x := 0 to bmp.Height - 1 do
    begin  // Walk scanlines
      O := bmp.ScanLine[x];       // New Target (Original)
      TT := tBufr.ScanLine[x];     //old x-2  (TopTop)
      T  := tBufr.ScanLine[x + 1]; //old x-1  (Top)
      C  := tBufr.ScanLine[x + 2]; //old x    (Center)
      B  := tBufr.ScanLine[x + 3]; //old x+1  (Bottom)
      BB := tBufr.ScanLine[x + 4]; //old x+2  (BottomBottom)
    // Now do the main piece
      for y := 2 to (tBufr.Width - 3) do
      begin  // Walk pixels
        O[y - 2].rgbtRed := Set255(
            ((TT[y - 2].rgbtRed * flt.RAY[0]) + (TT[y - 1].rgbtRed * flt.RAY[1]) +
             (TT[y    ].rgbtRed * flt.RAY[2]) + (TT[y + 1].rgbtRed * flt.RAY[3]) +
             (TT[y + 2].rgbtRed * flt.RAY[4]) +

             (T[y - 2].rgbtRed * flt.RAY[5]) + (T[y - 1].rgbtRed * flt.RAY[6]) +
             (T[y    ].rgbtRed * flt.RAY[7]) + (T[y + 1].rgbtRed * flt.RAY[8]) +
             (T[y + 2].rgbtRed * flt.RAY[9]) +

             (C[y - 2].rgbtRed * flt.RAY[10]) + (C[y - 1].rgbtRed * flt.RAY[11]) +
             (C[y    ].rgbtRed * flt.RAY[12]) + (C[y + 1].rgbtRed * flt.RAY[13]) +
             (C[y + 2].rgbtRed * flt.RAY[14]) +

             (B[y - 2].rgbtRed * flt.RAY[15]) + (B[y - 1].rgbtRed * flt.RAY[16]) +
             (B[y    ].rgbtRed * flt.RAY[17]) + (B[y + 1].rgbtRed * flt.RAY[18]) +
             (B[y + 2].rgbtRed * flt.RAY[19]) +

             (BB[y - 2].rgbtRed * flt.RAY[20]) + (BB[y - 1].rgbtRed * flt.RAY[21]) +
             (BB[y    ].rgbtRed * flt.RAY[22]) + (BB[y + 1].rgbtRed * flt.RAY[23]) +
             (BB[y + 2].rgbtRed * flt.RAY[24])) div flt.DIVFACTOR + flt.BIAS
            );


        O[y - 2].rgbtBlue := Set255(
            ((TT[y - 2].rgbtBlue * flt.RAY[0]) + (TT[y - 1].rgbtBlue * flt.RAY[1]) +
             (TT[y    ].rgbtBlue * flt.RAY[2]) + (TT[y + 1].rgbtBlue * flt.RAY[3]) +
             (TT[y + 2].rgbtBlue * flt.RAY[4]) +

             (T[y - 2].rgbtBlue * flt.RAY[5]) + (T[y - 1].rgbtBlue * flt.RAY[6]) +
             (T[y    ].rgbtBlue * flt.RAY[7]) + (T[y + 1].rgbtBlue * flt.RAY[8]) +
             (T[y + 2].rgbtBlue * flt.RAY[9]) +

             (C[y - 2].rgbtBlue * flt.RAY[10]) + (C[y - 1].rgbtBlue * flt.RAY[11]) +
             (C[y    ].rgbtBlue * flt.RAY[12]) + (C[y + 1].rgbtBlue * flt.RAY[13]) +
             (C[y + 2].rgbtBlue * flt.RAY[14]) +

             (B[y - 2].rgbtBlue * flt.RAY[15]) + (B[y - 1].rgbtBlue * flt.RAY[16]) +
             (B[y    ].rgbtBlue * flt.RAY[17]) + (B[y + 1].rgbtBlue * flt.RAY[18]) +
             (B[y + 2].rgbtBlue * flt.RAY[19]) +

             (BB[y - 2].rgbtBlue * flt.RAY[20]) + (BB[y - 1].rgbtBlue * flt.RAY[21]) +
             (BB[y    ].rgbtBlue * flt.RAY[22]) + (BB[y + 1].rgbtBlue * flt.RAY[23]) +
             (BB[y + 2].rgbtBlue * flt.RAY[24])) div flt.DIVFACTOR + flt.BIAS
            );

        O[y - 2].rgbtGreen := Set255(
            ((TT[y - 2].rgbtGreen * flt.RAY[0]) + (TT[y - 1].rgbtGreen * flt.RAY[1]) +
             (TT[y    ].rgbtGreen * flt.RAY[2]) + (TT[y + 1].rgbtGreen * flt.RAY[3]) +
             (TT[y + 2].rgbtGreen * flt.RAY[4]) +

             (T[y - 2].rgbtGreen * flt.RAY[5]) + (T[y - 1].rgbtGreen * flt.RAY[6]) +
             (T[y    ].rgbtGreen * flt.RAY[7]) + (T[y + 1].rgbtGreen * flt.RAY[8]) +
             (T[y + 2].rgbtGreen * flt.RAY[9]) +

             (C[y - 2].rgbtGreen * flt.RAY[10]) + (C[y - 1].rgbtGreen * flt.RAY[11]) +
             (C[y    ].rgbtGreen * flt.RAY[12]) + (C[y + 1].rgbtGreen * flt.RAY[13]) +
             (C[y + 2].rgbtGreen * flt.RAY[14]) +

             (B[y - 2].rgbtGreen * flt.RAY[15]) + (B[y - 1].rgbtGreen * flt.RAY[16]) +
             (B[y    ].rgbtGreen * flt.RAY[17]) + (B[y + 1].rgbtGreen * flt.RAY[18]) +
             (B[y + 2].rgbtGreen * flt.RAY[19]) +

             (BB[y - 2].rgbtGreen * flt.RAY[20]) + (BB[y - 1].rgbtGreen * flt.RAY[21]) +
             (BB[y    ].rgbtGreen * flt.RAY[22]) + (BB[y + 1].rgbtGreen * flt.RAY[23]) +
             (BB[y + 2].rgbtGreen * flt.RAY[24])) div flt.DIVFACTOR + flt.BIAS
            );

      end;
    end;
  finally
    tBufr.Free;
  end;
end;

function InvertBitmap(Bmp: TBitmap): boolean;
begin
    result := InvertBitmap(Bmp, Bmp);
end;

function InvertBitmap(OrigBmp, DestBmp: TBitmap): boolean;
begin
  // use of the GDI InvertRect() A>PI is even faster...
  result := InvertRect(OrigBmp.Canvas.Handle, OrigBmp.Canvas.ClipRect);
  DestBmp.Assign(OrigBmp);
end;

procedure CopySymbolColor(source, dest: TSymbolColor);
begin
  dest.ForeColor := source.ForeColor;
  dest.BackColor := source.BackColor;
  dest.Style := source.Style;
end;

procedure CopyHLColorSettings(source, dest: TRAHLEditor);
begin
  CopySymbolColor(source.Colors.Comment, dest.Colors.Comment);
  CopySymbolColor(source.Colors.Declaration, dest.Colors.Declaration);
  CopySymbolColor(source.Colors.FunctionCall, dest.Colors.FunctionCall);
  CopySymbolColor(source.Colors.Identifer, dest.Colors.Identifer);
  CopySymbolColor(source.Colors.Number, dest.Colors.Number);
  CopySymbolColor(source.Colors.PlainText, dest.Colors.PlainText);
  CopySymbolColor(source.Colors.Preproc, dest.Colors.Preproc);
  CopySymbolColor(source.Colors.Reserved, dest.Colors.Reserved);
  CopySymbolColor(source.Colors.Statement, dest.Colors.Statement);
  CopySymbolColor(source.Colors.Strings, dest.Colors.Strings);
  CopySymbolColor(source.Colors.Symbol, dest.Colors.Symbol);
  dest.RightMarginVisible := source.RightMarginVisible;
  dest.Color := source.Color;
  dest.Font := Source.Font;
  dest.Invalidate;
end;

procedure FlashComboBox(c: TComboBox);
begin
  if c.ItemIndex >= 0 then
    c.Text := c.Items.Strings[c.ItemIndex];
end;

function GetComboBoxIntValue(c: TComboBox): integer;
begin
  if c.ItemIndex = -1 then
    result := 0
  else
    result := StrToIntDef(c.Items.Values[c.Items.Names[c.ItemIndex]], 0);
end;

function GetComboBoxStrValue(c: TComboBox): string;
begin
  if c.ItemIndex = -1 then
    result := '0'
  else
    result := c.Items.Names[c.ItemIndex];
end;

function SetComboBoxIntValue(c: TComboBox; Value: integer): boolean;
var i: integer;
begin
  for i := 0 to c.Items.Count - 1 do
    if StrToIntDef(c.Items.Values[c.Items.Names[i]], 0) = Value then
    begin
      c.ItemIndex := i;
      FlashComboBox(c);
      result := true;
      exit;
    end;
  result := false;
end;

{$ENDIF}

end.
