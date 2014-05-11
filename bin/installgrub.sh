#!/bin/bash

EFI_DIR=/boot/efi
sudo grub-install --target=x86_64-efi --efi-directory=${EFI_DIR} --bootloader-id=arch --recheck
