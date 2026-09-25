# markermd 0.1.0

* Initial version.
* Raw blocks that are nothing but `<!-- ... -->` comments are now their own node kind, `HTML comment`, so rules and filters distinguish `{=html}` write-ups (still `Raw Block`) from skeleton placeholders and repair markers, and `assignment_outline()` no longer counts comments as content.
