unit modelhelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function BoolToVisibleString(value: boolean): String;
function TruncateMultilineText(const inputText: string; maxLineCount: Integer; replaceChar: Char = ' '): string;

implementation

//Converts a boolean value to a string for displaying in the GUI
//returns x for true and - for false
function BoolToVisibleString(value: boolean): String;
begin
  Result:= '-';
  if value = true then
  begin
    Result:= 'x';
  end;
end;


function TruncateMultilineText(const inputText: string; maxLineCount: Integer; replaceChar: Char = ' '): string;
var
  lines: TStringList;
  i: Integer;
begin
  // Split the input text into lines
  lines := TStringList.Create;
  try
    lines.Text := inputText;

    // Truncate if the number of lines exceeds maxLineCount
    if lines.Count > maxLineCount then
    begin
      for i := maxLineCount to lines.Count - 1 do
        lines[i] := ''; // Remove extra lines
    end;

    // Replace line breaks with the specified character
    Result := StringReplace(lines.Text, sLineBreak, replaceChar, [rfReplaceAll]);
  finally
    lines.Free;
  end;
end;



end.

