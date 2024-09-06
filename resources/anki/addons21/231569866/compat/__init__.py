from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from ..firstrun import Version


def compat(prev_version: "Version") -> None:
    """Executes code for compatability from older versions."""
    if prev_version == "-1.-1":
        # Newly installed
        return
