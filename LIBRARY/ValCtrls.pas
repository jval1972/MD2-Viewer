(***************************************************************************
 * Filename: ValCtrls.pas
 **************************************************************************)

unit ValCtrls;

interface

{$r VALCTRLSBTN.DCR}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Validations;

type

  TTimeEdit = class( TValidCustomEdit )
  private
    FShowSeconds: Boolean;             (* Show seconds in field *)
    FValue: TDateTime;                 (* Current time value *)
    { Private declarations }
    procedure SetValue( AValue: TDateTime );
    function GetValue: TDateTime;
    procedure SetSeconds( AValue: Boolean );
    procedure UpdateTime;
  protected
    { Protected declarations }
    function Validate: Boolean; override;
  public
    property IsValid;
    property IsEmpty;
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
  published
    { Published declarations }
    property AllowEmpty;
    property AutoSelect;
    property BorderStyle;
    property BeepOnInvalid;
    property OnInvalid;
    property ShowSeconds: Boolean read FShowSeconds write SetSeconds
      default True;
    property Value: TDateTime read GetValue write SetValue;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TIntegerEdit = class( TValidCustomEdit )
  private
    FMinValue: Longint;                (* Minimum value for field *)
    FMaxValue: Longint;                (* Maximum value for field *)
    FValue: Longint;                   (* Current value for field *)
    FUseBounds: Boolean;               (* Apply Min/Max bounds *)
    { Private declarations }
    procedure SetValue( AValue: Longint );
    function GetValue: Longint;
    procedure SetMinValue( AMinValue: Longint );
    procedure SetMaxValue( AMaxValue: Longint );
    procedure UpdateValue;
  protected
    { Protected declarations }
    function Validate: Boolean; override;
  public
    property IsValid;
    property IsEmpty;
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
  published
    { Published declarations }
    property MinValue: Longint read FMinValue write SetMinValue default 0;
    property MaxValue: Longint read FMaxValue write SetMaxValue default 0;
    property AutoSelect;
    property BorderStyle;
    property ReadOnly;
    property OnChange;
    property AllowEmpty;
    property BeepOnInvalid;
    property OnInvalid;
    property Value: Longint read GetValue write SetValue;
    property OnEnter;
    property OnExit;
    property UseBounds: Boolean read FUseBounds write FUseBounds
      default False;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TFloatView = ( fvGeneral, fvExponent, fvFixed );

  TFloatEdit = class( TValidCustomEdit )
  private
    FMinValue: Extended;               (* Minimum value for field *)
    FMaxValue: Extended;               (* Maximum value for field *)
    FValue: Extended;                  (* Current value for field *)
    FUseBounds: Boolean;               (* Apply Min/Max bounds *)
    FDigits: Integer;                  (* Max number for digits to show *)
    FPrecision: Integer;               (* Number of decimal digits to show *)
    FStrFormat: TFloatFormat;          (* Float format for FloatToStrF *)
    FFormat: TFloatView;               (* Float format *)
    { Private declarations }
    procedure SetValue( AValue: Extended );
    function GetValue: Extended;
    procedure SetMinValue( AMinValue: Extended );
    procedure SetMaxValue( AMaxValue: Extended );
    procedure SetDigits( Digits: Integer );
    procedure SetPrecision( Precision: Integer );
    procedure SetFormat( Format: TFloatView );
    procedure UpdateValue;
  protected
    { Protected declarations }
    function Validate: Boolean; override;
  public
    property IsValid;
    property IsEmpty;
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
  published
    { Published declarations }
    property MinValue: Extended read FMinValue write SetMinValue;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property Digits: Integer read FDigits write SetDigits default 10;
    property Precision: Integer read FPrecision write SetPrecision default 2;
    property Format: TFloatView read FFormat write SetFormat
      default fvGeneral;
    property AutoSelect;
    property BorderStyle;
    property ReadOnly;
    property OnChange;
    property AllowEmpty;
    property BeepOnInvalid;
    property OnInvalid;
    property Value: Extended read GetValue write SetValue;
    property OnEnter;
    property OnExit;
    property UseBounds: Boolean read FUseBounds write FUseBounds
      default False;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TCharSet = ( csSpace, csTab, csUpperChar, csLowerChar, csNumber );
  TCharSets = set of TCharSet;

  TCharEdit = class( TValidCustomEdit )
  private
    FCharSets: TCharSets;              (* What characters to allow *)
    FMinLength: Integer;               (* Minimum length of field *)
    FMaxLength: Integer;               (* Maximum length of field *)
    { Private declarations }
    procedure SetMinLength( AMinLength: Integer );
    procedure SetMaxLength( AMaxLength: Integer );
  protected
    { Protected declarations }
    function Validate: Boolean; override;
  public
    property IsValid;
    property IsEmpty;
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
  published
    { Published declarations }
    property CharSets: TCharSets read FCharSets write FCharSets default
      [ csSpace, csUpperChar, csLowerChar, csNumber ];
    property MinLength: Integer read FMinLength write SetMinLength default 1;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 10;
    property AutoSelect;
    property BorderStyle;
    property ReadOnly;
    property OnChange;
    property BeepOnInvalid;
    property OnInvalid;
    property Text;
    property OnEnter;
    property OnExit;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents( 'New', [ TTimeEdit, TFloatEdit, TIntegerEdit,
    TCharEdit ] );
end;

constructor TTimeEdit.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FShowSeconds := True;
  FValue := 0;
  UpdateTime;
end;

function TTimeEdit.Validate: Boolean;
begin
  try
    FValue := StrToTime( Text );
    UpdateTime;
    Result := True;
  except
    on EConvertError do
      Result := False;
  end;
end;

procedure TTimeEdit.SetValue( AValue: TDateTime );
begin
  FValue := AValue;
  UpdateTime;
end;

function TTimeEdit.GetValue: TDateTime;
begin
  if CheckValid then
    Result := FValue
  else
    raise EConvertError.Create( 'Invalid validation text' );
end;

procedure TTimeEdit.SetSeconds( AValue: Boolean );
begin
  if FShowSeconds <> AValue then
  begin
    FShowSeconds := AValue;
    UpdateTime;
  end;
end;

procedure TTimeEdit.UpdateTime;
begin
  if FShowSeconds then
    Text := FormatDateTime( 'tt', FValue )
  else
    Text := FormatDateTime( 't', FValue );
end;

{****************************************************************************}

constructor TIntegerEdit.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  UpdateValue;
end;

procedure TIntegerEdit.UpdateValue;
begin
  Text := IntToStr( FValue );
end;

function TIntegerEdit.Validate: Boolean;
begin
  try
    FValue := StrToInt( Text );
    UpdateValue;
    if FUseBounds and ( ( FValue < FMinValue ) or ( FValue > FMaxValue ) ) then
      Result := False
    else
      Result := True;
  except
    on EConvertError do
      Result := False;
  end;
end;

procedure TIntegerEdit.SetMinValue( AMinValue: Longint );
begin
  if FUseBounds and ( FValue < AMinValue ) then
    raise ERangeError.Create( 'Value out of bound' )
  else
    FMinValue := AMinValue;
end;

procedure TIntegerEdit.SetMaxValue( AMaxValue: Longint );
begin
  if FUseBounds and ( FValue > AMaxValue ) then
    raise ERangeError.Create( 'Value out of bound' )
  else
    FMaxValue := AMaxValue;
end;

procedure TIntegerEdit.SetValue( AValue: Longint );
begin
  if FUseBounds and ( ( AValue < MinValue ) or ( AValue > MaxValue ) ) then
    raise ERangeError.Create( 'Value out of bound' )
  else
  begin
    FValue := AValue;
    UpdateValue;
  end;
end;

function TIntegerEdit.GetValue: Longint;
begin
  if CheckValid then
    Result := FValue
  else
    raise EConvertError.Create( 'Invalid validation text' )
end;

{****************************************************************************}

constructor TFloatEdit.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FDigits := 10;
  FPrecision := 2;
  Format := fvGeneral;
end;

procedure TFloatEdit.UpdateValue;
begin
  Text := FloatToStrF( FValue, FStrFormat, FDigits, FPrecision );
end;

procedure TFloatEdit.SetDigits( Digits: Integer );
begin
  FDigits := Digits;
  UpdateValue;
end;

procedure TFloatEdit.SetPrecision( Precision: Integer );
begin
  FPrecision := Precision;
  UpdateValue;
end;

procedure TFloatEdit.SetFormat( Format: TFloatView );
begin
  FFormat := Format;
  case Format of
    fvGeneral: FStrFormat := ffGeneral;
    fvExponent: FStrFormat := ffExponent;
  else
    FStrFormat := ffFixed;
  end;
  UpdateValue;
end;

function TFloatEdit.Validate: Boolean;
begin
  try
    FValue := StrToFloat( Text );
    UpdateValue;
    if FUseBounds and ( ( FValue < FMinValue ) or ( FValue > FMaxValue ) ) then
      Result := False
    else
      Result := True;
  except
    on EConvertError do
      Result := False;
  end;
end;

procedure TFloatEdit.SetMinValue( AMinValue: Extended );
begin
  if FUseBounds and ( FValue < AMinValue ) then
    raise ERangeError.Create( 'Value out of bound' )
  else
    FMinValue := AMinValue;
end;

procedure TFloatEdit.SetMaxValue( AMaxValue: Extended );
begin
  if FUseBounds and ( FValue > AMaxValue ) then
    raise ERangeError.Create( 'Value out of bound' )
  else
    FMaxValue := AMaxValue;
end;

procedure TFloatEdit.SetValue( AValue: Extended );
begin
  if FUseBounds and ( ( AValue < MinValue ) or ( AValue > MaxValue ) ) then
    raise ERangeError.Create( 'Value out of bound' )
  else
  begin
    FValue := AValue;
    UpdateValue;
  end;
end;

function TFloatEdit.GetValue: Extended;
begin
  if CheckValid then
    Result := FValue
  else
    raise EConvertError.Create( 'Invalid validation text' )
end;

{****************************************************************************}

constructor TCharEdit.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FMinLength := 1;
  FMaxLength := 10;
  FCharSets := [ csSpace, csUpperChar, csLowerChar, csNumber ];
  AllowEmpty := False;
end;

function TCharEdit.Validate: Boolean;
var
  Len, Cnt: Integer;
begin
  Result := False;
  Len := Length( Text );
  if ( Len >= FMinLength ) and ( Len <= FMaxLength ) then
  begin
    if Len > 0 then
    begin
      for Cnt := 1 to Len do
        case Text[ Cnt ] of
          #9:
            if not ( csTab in FCharSets ) then
              exit;
          #32:
            if not ( csSpace in FCharSets ) then
              exit;
          'A'..'Z':
            if not ( csUpperChar in FCharSets ) then
              exit;
          'a'..'z':
            if not ( csLowerChar in FCharSets ) then
              exit;
          '0'..'9':
            if not ( csNumber in FCharSets ) then
              exit;
          else
            exit;
        end;
    end;
    Result := True;
  end;
end;

procedure TCharEdit.SetMinLength( AMinLength: Integer );
begin
  if ( AMinLength < 0 ) or ( AMinLength > FMaxLength ) then
    raise ERangeError.Create( 'Value out of bound' )
  else
    FMinLength := AMinLength;
end;

procedure TCharEdit.SetMaxLength( AMaxLength: Integer );
begin
  if ( AMaxLength > 255 ) or ( AMaxLength < FMinLength ) then
    raise ERangeError.Create( 'Value out of bound' )
  else
    FMaxLength := AMaxLength;
end;

end.
