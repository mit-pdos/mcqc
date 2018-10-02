Definition mksome {T} (a: T) : option T := Some a.

Require Extraction.
Extraction Language JSON.
Separate Extraction mksome.