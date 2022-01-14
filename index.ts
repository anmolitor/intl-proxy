const wrap = (fun: (arg: string) => any) =>
  new Proxy(
    {},
    {
      get(_target, arg) {
        return fun(arg as string);
      },
      has(_target, _arg) {
        return true;
      },
    }
  );

/**
 * An object enabling access to the Intl API via JSON. *
 * */
const intl_proxy = wrap((json) => {
  try {
    const [api, apiArgs, method, methodArgs] = JSON.parse(json);
    return new (Intl as any)[api](...apiArgs)[method](...methodArgs);
  } catch (_e) {
    return undefined;
  }
});

export default intl_proxy;
