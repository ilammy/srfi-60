def value(obj):
    if type(obj) == bool:
        return '#t' if obj else '#f'
    elif type(obj) == list:
        return "'(" + " ".join(map(value, obj)) + ")"
    else:
        return str(obj)

def procedure_name(name):
    name = name.replace('_', '-').replace('-to-', '->')
    if name[-1] == 'p':
        name = name[:-1] + '?'
    return name
