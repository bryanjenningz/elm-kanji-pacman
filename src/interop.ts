namespace ElmLand {
  export type FlagsFunction = ({
    env,
  }: {
    env: Record<string, string>;
  }) => unknown;

  export type OnReadyFunction = ({
    env,
    app,
  }: {
    env: Record<string, string>;
    app: { ports?: Record<string, Port> };
  }) => void;

  export type Port = {
    subscribe?: (callback: (data: unknown) => void) => void;
    unsubscribe?: (callback: (data: unknown) => void) => void;
    send?: (data: unknown) => void;
  };
}

export const flags: ElmLand.FlagsFunction = () => {};

export const onReady: ElmLand.OnReadyFunction = ({ app }) => {
  app.ports?.outgoing?.subscribe?.((message: unknown) => {
    console.log({ message });
  });
};
