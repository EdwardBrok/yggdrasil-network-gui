unit unitpasswordinput;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TFormPasswordInput }

  TFormPasswordInput = class(TForm)
    CancelButton: TButton;
    PasswordInputLabel: TLabel;
    PasswordsAreNotCollectedLabel: TLabel;
    CommentLabel: TLabel;
    OKButton: TBitBtn;
    PasswordEdit: TEdit;
    procedure OKButtonClick(Sender: TObject);
  private
  public
    Password: string;
  end;

var
  FormPasswordInput: TFormPasswordInput;


implementation

{$R *.lfm}

{ TFormPasswordInput }

procedure TFormPasswordInput.OKButtonClick(Sender: TObject);
begin
  Password := PasswordEdit.Text + #$0A;
end;

initialization

end.

