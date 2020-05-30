(***************************************************************************
 * Filename: Validations.pas
 *   Author: Peter Andersson
 *  Created: 1996.April.20
 *  Updated: -
 ***************************************************************************
 * (C) Copyright Peter Andersson, 1996. All rights reserved.
 **************************************************************************)

unit Validations;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type

  TInvalidEvent = procedure ( Sender: TObject; var CanLeave: Boolean )
    of object;

  TValidCustomEdit = class( TCustomEdit )
  private
    FIsValid: Boolean;                 (* is the field is valid *)
    FIsEmpty: Boolean;                 (* is the field is empty *)
    FAllowEmpty: Boolean;              (* is the field allowed to be empty *)
    FBeepOnInvalid: Boolean;           (* beep if invalid *)
    FOnInvalid: TInvalidEvent;         (* invalid event *)
    { Private declarations }
    procedure CMExit( var Message: TCMExit ); message CM_EXIT;
  protected
    property IsValid: Boolean read FIsValid;
    property IsEmpty: Boolean read FIsEmpty;
    property AllowEmpty: Boolean read FAllowEmpty write FAllowEmpty
      default True;
    property BeepOnInvalid: Boolean read FBeepOnInvalid write FBeepOnInvalid
      default True;
    property OnInvalid: TInvalidEvent read FOnInvalid write FOnInvalid;
    { Protected declarations }
    function CheckValid: Boolean;
    function Validate: Boolean; dynamic; abstract;
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
  published
    { Published declarations }
  end;

implementation

constructor TValidCustomEdit.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FAllowEmpty := True;
  FBeepOnInvalid := True;
end;

function TValidCustomEdit.CheckValid: Boolean;
begin
  FIsEmpty := Length( Text ) = 0;
  if FAllowEmpty and FIsEmpty then
    Result := True
  else
  begin
    FIsValid := Validate;
    Result := FIsValid;
    if not FIsValid then
    begin
      if FBeepOnInvalid then
        MessageBeep( MB_ICONEXCLAMATION	);
      if Assigned( FOnInvalid ) then
        FOnInvalid( Self, Result );
    end;
  end;
end;

procedure TValidCustomEdit.CMExit( var Message: TCMExit );
begin
  if not ( csDesigning in ComponentState ) and not CheckValid then
    SetFocus
  else
    inherited;
end;

end.
