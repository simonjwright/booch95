with BC.Containers;
with BC.Containers.Lists;
with BC.Containers.Lists.Single;
with BC.Containers.Lists.Double;
with BC.Support.Managed_Storage;
with BC.Support.Unmanaged_Storage;
package Lists_For_Timing is
  type Integer_P is access all Integer;
  package C is new BC.Containers (Integer, Integer_P);
  package L is new C.Lists;
  P : BC.Support.Managed_Storage.Pool (16#100_000#);
  package S is new L.Single (BC.Support.Managed_Storage.Pool, P);
  package D is new L.Double (BC.Support.Managed_Storage.Pool, P);
--   P : BC.Support.Unmanaged_Storage.Pool;
end Lists_For_Timing;
