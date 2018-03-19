Rust module example
-------------------

On Fedora 27:

Install Rust and Wilton:

    cat /etc/redhat-release
    > Fedora release 27 (Twenty Seven)
    sudo dnf install rust cargo
    sudo dnf copr enable wilton/wilton 
    sudo dnf install wilton

Build and run the Rust module example:

    git clone https://github.com/wilton-iot/wilton_examples.git
    cd wilton_examples/rust/
    cargo build
    wilton test.js 
    > Calling Rust lib ...
    > Call response: [{
    >  "boo": "Hello from Rust lib!",
    >  "baz": 43
    >}]

