With Ada.Text_Io;
with Ada.Integer_Text_Io;
with Ada.Unchecked_Deallocation;
with BC.Support.Managed_Storage;
with BC.Support.Unmanaged_Storage;

procedure Storage is

  Unmanaged_Pool : BC.Support.Unmanaged_Storage.Pool;
  Managed_Pool : BC.Support.Managed_Storage.Pool (4096);

  type T is array (Integer range <>) of Character;
  type U_P is access T;
  for U_P'Storage_Pool use Unmanaged_Pool;
  type M_P is access T;
  for M_P'Storage_Pool use Managed_Pool;

  procedure Delete is new Ada.Unchecked_Deallocation (T, U_P);
  procedure Delete is new Ada.Unchecked_Deallocation (T, M_P);

  U_Ptr : U_P;
  M_Ptr : M_P;

  Trying : Integer;

begin

  Ada.Text_Io.Put_Line ("trying unmanaged store:");
  for I in 0 .. 4096 loop
    Trying := I;
    U_Ptr := new T (1 .. Trying);
    Delete (U_Ptr);
  end loop;
  Ada.Text_Io.Put_Line (".. done.");

  Ada.Text_Io.Put_Line ("trying managed store:");
    for I in 0 .. 4096 loop
    Trying := I;
    M_Ptr := new T (1 .. Trying);
    Delete (M_Ptr);
  end loop;
  Ada.Text_Io.Put_Line (".. done.");

exception
  when BC.Storage_Error =>
    Ada.Text_Io.Put ("exception BC.Storage_Error raised at size");
    Ada.Integer_Text_Io.Put (Trying);
    Ada.Text_Io.New_Line;
end Storage;
