unit usettingitemuridto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uSettingItemBase;

type
TSettingItemUriDto = class(TSettingItemBaseDto)
  private
    FsUri: string;
  published
    property uri: string read FsUri write FsUri;
  end;


implementation

end.

