Normalise git mailmap files

```
mailmap <<eof
John Doe <john-doe@example.com> <john-doh@example.com>
John Doe <john-doe@example.com> <john-joe@example.com>
Oneliner <0@example.com> <1@example.com> <2@example.com> <3@example.com>
Single <single@example.com>
eof
```

Outputs:

```
Oneliner <0@example.com> <1@example.com>
Oneliner <0@example.com> <2@example.com>
Oneliner <0@example.com> <3@example.com>
John Doe <john-doe@example.com> <john-doh@example.com>
John Doe <john-doe@example.com> <john-joe@example.com>
Single <single@example.com>
```
